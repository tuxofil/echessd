%% @doc
%% Language info keeper process.

%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%% @since 22 Nov 2012
%% @copyright 2013, Aleksey Morarash

-module(echessd_lang).

-behaviour(gen_server).

%% API exports
-export([start_link/0, gettext/1, gettext/2, list/0, parse/1]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-type lang_config() ::
        {Languages :: [{LangID :: atom(),
                        LangName :: nonempty_string()}],
         TextItems :: [lang_config_text_item()]}.

-type lang_config_text_item() ::
        {TextItemID :: atom(), LangID2StringMap :: dict()}.

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, _Args = undefined, _Options = []).

%% @equiv gettext(TextID, undefined)
%% @doc Fetch localized text for the given Text ID.
-spec gettext(TextID :: any()) -> LocalizedText :: string().
gettext(TextID) ->
    gettext(TextID, undefined).

%% @doc Fetch localized text for the given Text ID.
-spec gettext(TextID :: atom(),
              LangID :: atom() | undefined) ->
                     LocalizedText :: string().
gettext(TextID, undefined) ->
    case echessd_cfg:get(?CFG_DEF_LANG) of
        undefined ->
            throw(?e_undefined_language);
        LangID ->
            gettext(TextID, LangID)
    end;
gettext(TextID, LangID) ->
    [{TextID, Dict}] = ets:lookup(?MODULE, TextID),
    case dict:find(LangID, Dict) of
        {ok, Text} ->
            Text;
        _ ->
            DefLang = echessd_cfg:get(?CFG_DEF_LANG),
            {ok, Text} = dict:find(DefLang, Dict),
            Text
    end.

%% @doc Return a list of available languages.
-spec list() -> [{LangID :: atom(), LangName :: nonempty_string()}].
list() ->
    [{languages, List}] = ets:lookup(?MODULE, languages),
    List.

%% @doc Parse the Language ID to the internal representation.
-spec parse(String :: string()) -> LangID :: atom().
parse([_ | _] = String) ->
    Langs = [LangID || {LangID, _LangName} <- list()],
    case echessd_lib:list_to_atom(String, Langs) of
        {ok, LangID} ->
            LangID;
        error ->
            echessd_cfg:get(?CFG_DEF_LANG)
    end;
parse(_Bad) ->
    echessd_cfg:get(?CFG_DEF_LANG).

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    {Languages, Texts} = do_read(),
    true = ets:insert(?MODULE, [{languages, Languages} | Texts]),
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

%% @doc Read languages definitions from the priv/echessd.lang file.
-spec do_read() -> lang_config().
do_read() ->
    {ok, Binary} = echessd_priv:read_file("echessd.lang"),
    List = echessd_lib:string_to_terms(binary_to_list(Binary)),
    {proplists:get_value(languages, List),
     [{ID, dict:from_list(L)} || {text, ID, L} <- List]}.
