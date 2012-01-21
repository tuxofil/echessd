-module(echessd_session).

-export([init/0, sid/0, mk/1, get/1, del/1,
         read/1,
         get_val/1,
         set_val/2
        ]).

-include("echessd.hrl").

%% @doc Creates non-persistent storage of user current sessions.
%% @spec init() -> ok
init() ->
    ?dbt_session =
        ets:new(?dbt_session, [named_table, public, set]),
    ok.

sid() ->
    random:seed(now()),
    integer_to_list(random:uniform(1000000000000000000)).

mk(User) ->
    ets:insert(?dbt_session, [{SID = sid(), User, ""}]),
    SID.

get(SID) ->
    case ets:lookup(?dbt_session, SID) of
        [{_, User, Vars}] ->
            {ok, User, Vars};
        _ ->
            undefined
    end.

del(SID) ->
    true = ets:delete(?dbt_session, SID),
    ok.

read(Cookie) ->
    put(logged_in, false),
    erase(sid),
    erase(username),
    erase(userinfo),
    [erase(K) || {{session_var, _} = K, _} <- erlang:get()],
    SID = proplists:get_value("sid", Cookie),
    case echessd_session:get(SID) of
        {ok, User, Vars} ->
            case echessd_user:getprops(User) of
                {ok, Props} ->
                    put(logged_in, true),
                    put(sid, SID),
                    put(username, User),
                    put(userinfo, Props),
                    lists:foreach(
                      fun({K, V}) ->
                              put({session_var, K}, V)
                      end, Vars);
                _ ->
                    echessd_session:del(SID)
            end;
        _ -> nop
    end, ok.

get_val(Key) ->
    erlang:get({session_var, Key}).

set_val(Key, Val) ->
    SID = erlang:get(sid),
    case ?MODULE:get(SID) of
        {ok, User, Vars} ->
            put({session_var, Key}, Val),
            NewVars = [{Key, Val} | [I || {K, _} = I <- Vars, K /= Key]],
            ets:insert(?dbt_session, {SID, User, NewVars}),
            ok;
        _ ->
            throw(no_session)
    end.

