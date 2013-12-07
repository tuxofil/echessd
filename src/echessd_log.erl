%%% @doc
%%% Logger process.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_log).

-behaviour(gen_server).

%% API exports
-export([start_link/0, err/2, info/2, debug/2, hup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the logger process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Log the message with 'ERROR' severity.
-spec err(Format :: string(), Args :: list()) -> ok.
err(Format, Args) ->
    log(?LOG_ERR, Format, Args).

%% @doc Log the message with 'INFO' severity.
-spec info(Format :: string(), Args :: list()) -> ok.
info(Format, Args) ->
    log(?LOG_INFO, Format, Args).

%% @doc Log the message with 'DEBUG' severity.
-spec debug(Format :: string(), Args :: list()) -> ok.
debug(Format, Args) ->
    log(?LOG_DEBUG, Format, Args).

%% @doc Tell the logger to reconfigure and reopen log file.
-spec hup() -> ok.
hup() ->
    ok = gen_server:cast(?MODULE, hup).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state,
        {loglevel :: non_neg_integer(),
         file_descr :: io:device() | undefined}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    false = process_flag(trap_exit, true),
    ok = hup(),
    {ok, #state{}}.

%% @hidden
-spec handle_cast(Request :: hup, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(hup, State) ->
    {noreply, do_reconfig(State)}.

%% @hidden
-spec handle_info(Info :: {log,
                           Time :: erlang:timestamp(),
                           Severity :: echessd_config_parser:loglevel(),
                           Format :: string(), Args :: list()},
                  State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info({log, Time, Severity, Format, Args}, State)
  when State#state.file_descr /= undefined ->
    case loglevel_to_integer(Severity) =< State#state.loglevel of
        true ->
            IoList = format_msg(Time, Severity, Format, Args),
            _Ignored = file:write(State#state.file_descr, IoList),
            ok;
        false ->
            ok
    end,
    {noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(Reason, State) ->
    _Ignored =
        file:write(
          IoDevice = State#state.file_descr,
          format_msg(now(), ?LOG_INFO, "Terminating (~9999p)", [Reason])),
    catch file:close(IoDevice),
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc
-spec log(Severity :: echessd_config_parser:loglevel(),
          Format :: string(), Args :: list()) -> ok.
log(Severity, Format, Args) ->
    catch ?MODULE ! {log, os:timestamp(), Severity, Format, Args},
    ok.

%% @doc
-spec loglevel_to_integer(echessd_config_parser:loglevel()) ->
                                 non_neg_integer().
loglevel_to_integer(?LOG_ERR) ->
    1;
loglevel_to_integer(?LOG_INFO) ->
    2;
loglevel_to_integer(?LOG_DEBUG) ->
    3.

%% @doc Format the log message.
-spec format_msg(Time :: erlang:timestamp(),
                 Severity :: echessd_config_parser:loglevel(),
                 Format :: string(), Args :: list()) -> iolist().
format_msg(Time, Severity, Format, Args) ->
    try
        format_msg_(Time, Severity, Format, Args)
    catch
        Type:Reason ->
            format_msg_(
              Time, ?LOG_ERR, "bad log message ~99999p: ~99999p",
              [{Severity, Format, Args}, {Type, Reason}])
    end.

%% @doc
-spec format_msg_(Time :: erlang:timestamp(),
                  Severity :: echessd_config_parser:loglevel(),
                  Format :: string(), Args :: list()) -> iolist().
format_msg_(Time, Severity, Format, Args) ->
    io_lib:format(
      "~s ~s: " ++ Format ++ "~n",
      [echessd_lib:timestamp(Time),
       string:to_upper(atom_to_list(Severity)) | Args]).

%% @doc
-spec do_reconfig(State :: #state{}) -> NewState :: #state{}.
do_reconfig(State) ->
    {ok, ConfigPath} = application:get_env(?CFG_CONFIG_PATH),
    Config = echessd_config_parser:read(ConfigPath),
    Loglevel = proplists:get_value(?CFG_LOGLEVEL, Config),
    LogPath = proplists:get_value(?CFG_LOGFILE, Config),
    State#state{
      loglevel = loglevel_to_integer(Loglevel),
      file_descr = openlog(State#state.file_descr, LogPath)
     }.

%% @doc
-spec openlog(OldIoDevice :: io:device() | undefined,
              LogPath :: nonempty_string() | undefined) ->
                     NewIoDevice :: io:device() | undefined.
openlog(OldIoDevice, undefined) ->
    catch file:close(OldIoDevice),
    undefined;
openlog(OldIoDevice, [_ | _] = LogPath) ->
    catch file:close(OldIoDevice),
    catch filelib:ensure_dir(LogPath),
    case file:open(LogPath, [raw, append]) of
        {ok, IoDevice} ->
            IoDevice;
        {error, Reason} ->
            io:format(
              standard_error,
              format_msg(
                now(), ?LOG_ERR, "Unable to open ~9999p: ~p",
                [LogPath, Reason])),
            undefined
    end.
