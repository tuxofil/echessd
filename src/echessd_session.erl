%%% @doc
%%% HTTP session keeper process.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 21 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_session).

-behaviour(gen_server).

%% API exports
-export([start_link/0, new/1, get/1, save/1, del/1, from_cookies/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-export_type([id/0]).

-type id() :: nonempty_string().

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Start the session keeper process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Create a new session for the user.
-spec new(Username :: echessd_user:name() | undefined) ->
                 Session :: #session{}.
new(Username) ->
    #session{
     id = generate_sid(),
     created = os:timestamp(),
     username = Username
    }.

%% @doc Fetch session data.
-spec get(SID :: id()) -> Session :: #session{}.
get(SID) ->
    case ets:lookup(?MODULE, SID) of
        [Session] ->
            fill_session(Session);
        [] ->
            new(undefined)
    end.

%% @doc Save the session.
-spec save(Session :: #session{}) -> ok.
save(Session) ->
    true = ets:insert(?MODULE, Session#session{userinfo = []}),
    ok.

%% @doc Remove the session (logout user).
-spec del((SessionID :: id()) |
          (Session :: #session{})) -> ok.
del([_ | _] = SessionID) ->
    true = ets:delete(?MODULE, SessionID),
    ok;
del(Session) when is_record(Session, session) ->
    del(Session#session.id).

%% @doc Restore the session from the Cookie.
-spec from_cookies(Cookies :: echessd_httpd:cookies()) ->
                          Session :: #session{}.
from_cookies(Cookies) ->
    Session = echessd_session:get(proplists:get_value("sid", Cookies)),
    case Session#session.username of
        [_ | _] ->
            %% authenticated
            Session;
        undefined ->
            %% Not authenticated. Read settings from the cookies:
            StrLangID = proplists:get_value("lang", Cookies),
            StrStyleID = proplists:get_value("style", Cookies),
            Session#session{
              language = echessd_lang:parse(StrLangID),
              style = echessd_styles:parse(StrStyleID)
             }
    end.

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state, {}).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    ?MODULE = ets:new(?MODULE, [named_table, {keypos, 2}, public]),
    {ok, #state{}}.

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

%% @doc Generate user session ID.
-spec generate_sid() -> id().
generate_sid() ->
    _OldSeed = random:seed(now()),
    erlang:integer_to_list(random:uniform(16#ffffffffffffffff), 16).

%% @doc Fill the session object with data from user's account
%% (for logged in user).
-spec fill_session(Session :: #session{}) -> FilledSession :: #session{}.
fill_session(Session) when Session#session.username /= undefined ->
    case echessd_user:getprops(Session#session.username) of
        {ok, UserInfo} ->
            Session#session{
              userinfo = UserInfo,
              timezone = echessd_user:get_value(timezone, UserInfo),
              language = echessd_user:get_value(language, UserInfo),
              style    = echessd_user:get_value(style, UserInfo)
             };
        _ ->
            ok = del(Session),
            new(undefined)
    end;
fill_session(Session) ->
    Session.
