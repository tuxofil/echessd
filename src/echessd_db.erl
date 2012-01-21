-module(echessd_db).

-export([init/0, wait/0,
         mksession_table/0,
         adduser/2,
         deluser/1,
         get_user_props/1
        ]).

-include("echessd.hrl").

%% db table record
-record(hrec, {key, val}).

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

init() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    DbOpts =
        [{type, set},
         {record_name, hrec},
         {disc_copies, [node()]}],
    {atomic, ok} =
        mnesia:create_table(?dbt_users, DbOpts),
    {atomic, ok} =
        mnesia:create_table(?dbt_games, DbOpts),
    ok.

wait() ->
    case mnesia:wait_for_tables(
           [?dbt_users, ?dbt_games], 10000) of
        ok -> ok;
        {timeout, BadTabList} = Error ->
            echessd_log:err(
              "Timeout waiting DB tables: ", [BadTabList]),
            throw(Error);
        {error, Reason} ->
            echessd_log:err(
              "Error occured while waiting DB tables: ", [Reason]),
            throw(Reason)
    end.

%% @doc Creates non-persistent storage of user current sessions.
%% @spec mksession_table() -> ok
mksession_table() ->
    ?dbt_session =
        ets:new(?dbt_session, [named_table, public, set]),
    ok.

adduser(Name, Props) ->
    transaction(
      fun() ->
              case mnesia:read({?dbt_users, Name}) of
                  [_] ->
                      mnesia:abort(user_already_exists);
                  _ ->
                      mnesia:write(
                        ?dbt_users,
                        #hrec{key = Name,
                              val = Props},
                        write)
              end
      end).

deluser(Name) ->
    transaction(
     fun() ->
             mnesia:delete({?dbt_users, Name})
     end).

get_user_props(Name) ->
    transaction(
     fun() ->
             case mnesia:read({?dbt_users, Name}) of
                 [HRec] -> HRec#hrec.val;
                 _ -> mnesia:abort(no_such_user)
             end
     end).

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

