%% @doc
%% MIME types reference keeper process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 21 Nov 2013
%% @copyright 2012, Aleksey Morarash

-module(echessd_mime_types).

-behaviour(gen_server).

%% API exports
-export([start_link/0, lookup/1, hup/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Internal signals and keywords
%% ----------------------------------------------------------------------

-define(CAST_HUP, hup).
-define(CALL_GET_STATE, get_state).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the process as part of the supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, _Args = undefined, _Options = []).

%% @doc Search mime type for the file extension.
-spec lookup(Extension :: string()) -> MimeType :: string().
lookup(Extension) ->
    case ets:lookup(?MODULE, string:to_lower(Extension)) of
        [{_, MimeType}] ->
            MimeType;
        _ ->
            "application/octet-stream"
    end.

%% @doc Schedule configuration reload.
-spec hup() -> ok.
hup() ->
    gen_server:cast(?MODULE, ?CAST_HUP).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    ok = hup(),
    {ok, _State = #state{}}.

%% @hidden
-spec handle_cast(Request :: ?CAST_HUP, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?CAST_HUP, State) ->
    {ok, List} = read_mime_types(echessd_cfg:get(?CFG_MIME_TYPES)),
    true = ets:insert(?MODULE, [{E, T} || {T, L} <- List, E <- L]),
    {noreply, State}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Read and parse mime types file.
-spec read_mime_types(Filename :: file:filename()) ->
                             {ok, List :: [{MimeType :: nonempty_string(),
                                            Extensions :: [nonempty_string()]}]} |
                             {error, Reason :: any()}.
read_mime_types(Filename) ->
    case file:open(Filename, [read, raw, read_ahead]) of
        {ok, FH} ->
            Result =
                try read_mime_types_loop(FH, [])
                catch
                    Type:Reason:StackTrace ->
                        {error,
                         {Type, Reason, StackTrace}}
                end,
            catch file:close(FH),
            Result;
        Error -> Error
    end.

-spec read_mime_types_loop(FH :: file:io_device(),
                           Acc ::
                             [{MimeType :: nonempty_string(),
                               Extensions :: [nonempty_string()]}]) ->
                                  {ok,
                                   List ::
                                     [{MimeType :: nonempty_string(),
                                       Extensions :: [nonempty_string()]}]} |
                                  {error, Reason :: any()}.
read_mime_types_loop(FH, Acc) ->
    case file:read_line(FH) of
        eof ->
            {ok, lists:reverse(Acc)};
        {ok, Line} ->
            case echessd_lib:strip(Line, " \t\r\n") of
                [C | _] = Stripped when C /= $# ->
                    [MimeType | Extensions] =
                        string:tokens(Stripped, " \t"),
                    read_mime_types_loop(
                      FH, [{MimeType, Extensions} | Acc]);
                _ ->
                    read_mime_types_loop(FH, Acc)
            end;
        Error ->
            Error
    end.
