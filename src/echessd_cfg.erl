%%% @doc
%%% Echessd configuration keeper process.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_cfg).

-behaviour(gen_server).

%% API exports
-export([start_link/0, get/1, hup/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, _Args = undefined, _Options = []).

%% @doc Return a value for the configuration key.
-spec get(Key :: echessd_config_parser:config_key()) -> Value :: any().
get(Key) ->
    [{Key, Value}] = ets:lookup(?MODULE, Key),
    Value.

%% @doc Tell the process to reread the configuration file.
-spec hup() -> ok.
hup() ->
    ok = gen_server:cast(?MODULE, hup).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    ok = read(),
    {ok, _State = #state{}}.

%% @hidden
-spec handle_cast(Request :: hup, State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(hup, State) ->
    ok = read(),
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

%% @doc
-spec read() -> ok.
read() ->
    {ok, ConfigPath} = application:get_env(?CFG_CONFIG_PATH),
    Config = echessd_config_parser:read(ConfigPath),
    true = ets:insert(?MODULE, Config),
    ok.
