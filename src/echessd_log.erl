%%%-------------------------------------------------------------------
%%% File    : echessd_log.erl
%%% Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
%%% Created : 20 Jan 2012
%%% License : FreeBSD
%%% Description : logger process
%%%
%%%-------------------------------------------------------------------

-module(echessd_log).

-behaviour(gen_server).

%% API exports
-export([start_link/0,
         err/1, err/2,
         info/1, info/2,
         debug/1, debug/2,
         reopen/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts logger process as part of a supervision tree.
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Logs message with level 'ERROR'. When log file is not opened
%%      yet this data will be output to stdout.
%% @spec err(Text) -> ok
%% @spec err(Format, Args) -> ok
%%     Text = string(),
%%     Format = string(),
%%     Args = list()
err(String) ->
    log(?LOG_ERR, String).
err(Format, Args) ->
    log(?LOG_ERR, Format, Args).

%% @doc Logs message with level 'INFO'. When log file is not opened
%%      yet this data will be output to stdout.
%% @spec info(Text) -> ok
%% @spec info(Format, Args) -> ok
%%     Text = string(),
%%     Format = string(),
%%     Args = list()
info(String) ->
    log(?LOG_INFO, String).
info(Format, Args) ->
    log(?LOG_INFO, Format, Args).

%% @doc Logs message with level 'DEBUG'. This message will be
%%      silently discarded if log file is not opened yet.
%% @spec debug(Text) -> ok
%% @spec debug(Format, Args) -> ok
%%     Text = string(),
%%     Format = string(),
%%     Args = list()
debug(String) ->
    log(?LOG_DEBUG, String).
debug(Format, Args) ->
    log(?LOG_DEBUG, Format, Args).

%% @doc Sends signal to logger to reopen log file.
%% @spec reopen() -> ok
reopen() ->
    gen_server:cast(?MODULE, reopen),
    ok.

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {file_descr}).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{file_descr = openlog()}}.

handle_cast({log, T, MC, F, A}, State) ->
    file:write(
      State#state.file_descr,
      format_msg(T, MC, F, A)),
    {noreply, State};
handle_cast(reopen, State) ->
    {noreply,
     State#state{file_descr = openlog(State#state.file_descr)}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    IoDevice = State#state.file_descr,
    file:write(
      IoDevice,
      format_msg(
        now(), ?LOG_INFO,
        "Terminating (~9999p)", [Reason])),
    catch file:close(IoDevice),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

log(MessageClass, String) ->
    log(MessageClass, "~s", [String]).

log(MessageClass, Format, Args) ->
    LogLevel =
        try echessd_cfg:get(?CFG_LOGLEVEL)
        catch
            _:_ ->
                {ok, LogLevel0} =
                    echessd_cfg:default(?CFG_LOGLEVEL),
                LogLevel0
        end,
    if
        (LogLevel == ?LOG_DEBUG
         andalso
           (MessageClass == ?LOG_DEBUG orelse
            MessageClass == ?LOG_INFO orelse
            MessageClass == ?LOG_ERR))
        orelse
        (LogLevel == ?LOG_INFO
         andalso
           (MessageClass == ?LOG_INFO orelse
            MessageClass == ?LOG_ERR))
        orelse
        (LogLevel == ?LOG_ERR
         andalso
           (MessageClass == ?LOG_ERR)) ->
            Time = now(),
            try
                gen_server:cast(
                  ?MODULE,
                  {log, Time, MessageClass, Format, Args})
            catch
                _:_ when MessageClass /= ?LOG_DEBUG ->
                    %% logger is dead. write to stdout
                    io:format(
                      format_msg(
                        Time, MessageClass, Format, Args));
                _:_ -> ok
            end;
        true -> ok
    end.

openlog() ->
    openlog(undefined).
openlog(OldIoDevice) ->
    catch file:close(OldIoDevice),
    LogFilename = echessd_cfg:get(?CFG_LOGFILE),
    catch filelib:ensure_dir(LogFilename),
    case file:open(LogFilename, [raw, append]) of
        {ok, IoDevice} ->
            IoDevice;
        {error, Reason} ->
            io:format(
              format_msg(
                now(), ?LOG_ERR, "Unable to open ~9999p: ~9999p",
                [LogFilename, Reason])),
            throw(Reason)
    end.

format_msg(Time, MessageClass, Format, Args) ->
    try format_msg_(Time, MessageClass, Format, Args)
    catch
        Type:Reason ->
            format_msg_(
              Time, ?LOG_ERR, "bad log message ~99999p: ~99999p",
              [{MessageClass, Format, Args},
               {Type, Reason}])
    end.
format_msg_(Time, MessageClass, Format, Args) ->
    io_lib:format(
      "~s ~s: " ++ Format ++ "~n",
      [echessd_lib:timestamp(Time),
       string:to_upper(atom_to_list(MessageClass)) |
       Args]).

