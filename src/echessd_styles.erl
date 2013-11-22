%% @doc
%% CSS Styles keeper process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 22 Nov 2012
%% @copyright 2013, Aleksey Morarash

-module(echessd_styles).

-behaviour(gen_server).

%% API exports
-export([start_link/0, get/1, list/0, parse/1]).

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

%% @doc Return style info.
-spec get(StyleID :: atom()) -> {CaptionTextID :: atom(),
                                 Filename :: file:filename()}.
get(StyleID) ->
    [{StyleID, CaptionTextID, Filename}] = ets:lookup(?MODULE, StyleID),
    {CaptionTextID, Filename}.

%% @doc Return a list of available styles.
-spec list() -> [{StyleID :: atom(), CaptionTextID :: atom()}].
list() ->
    [{StyleID, CaptionTextID} ||
        {StyleID, CaptionTextID, _Filename} <- ets:tab2list(?MODULE)].

%% @doc Parse the Style ID to the internal representation.
-spec parse(String :: string()) -> StyleID :: atom().
parse([_ | _] = String) ->
    Styles = [StyleID || {StyleID, _TextID} <- list()],
    case echessd_lib:list_to_atom(String, Styles) of
        {ok, StyleID} ->
            StyleID;
        error ->
            echessd_cfg:get(?CFG_DEF_STYLE)
    end;
parse(_Bad) ->
    echessd_cfg:get(?CFG_DEF_STYLE).

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
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(_Request, State) ->
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
    {reply, State, State}.

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

%% @doc Read, parse and store CSS Style definitions.
-spec read() -> ok.
read() ->
    {ok, Binary} = echessd_priv:read_file("echessd.styles"),
    List = echessd_lib:string_to_terms(binary_to_list(Binary)),
    Tuples =
        [{ID, proplists:get_value(text_id, L),
          proplists:get_value(filename, L)} ||
            {style, ID, L} <- List],
    true = ets:delete_all_objects(?MODULE),
    true = ets:insert(?MODULE, Tuples),
    ok.
