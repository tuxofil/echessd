%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 18 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc gameplay notifications tools.

-module(echessd_notify).

-export([ply/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Dispatches notifications about ply of Username in game GameID.
%% @spec ply(GameID, Username, Ply) -> ok
%%     GameID = echessd_game:echessd_game_id(),
%%     Username = echessd_user:echessd_username(),
%%     Ply = echessd_game:echessd_ply()
ply(GameID, Username, Ply) ->
    case echessd_cfg:get(?CFG_XMPP_ENABLED) of
        true ->
            XmppUser = echessd_cfg:get(?CFG_XMPP_USER),
            XmppServer = echessd_cfg:get(?CFG_XMPP_SERVER),
            case {XmppUser, XmppServer} of
                {[_ | _], [_ | _]} ->
                    spawn(
                      fun() ->
                              do_ply(
                                GameID, Username, Ply,
                                XmppUser, XmppServer)
                      end);
                _ -> nop
            end;
        _ -> nop
    end,
    ok.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

do_ply(GameID, Username, Ply, XmppUser, XmppServer) ->
    XmppPassword = echessd_cfg:get(?CFG_XMPP_PASSWORD),
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Users = proplists:get_value(users, GameInfo, []),
    Watchers =
        [N || {N, _Role} <- Users, N /= Username],
    JIDs =
        lists:flatmap(
          fun(Watcher) ->
                  case echessd_user:getprops(
                         Watcher) of
                      {ok, WatcherInfo} ->
                          Lang =
                              proplists:get_value(
                                language, WatcherInfo),
                          case proplists:get_value(
                                 jid, WatcherInfo) of
                              [_ | _] = JID ->
                                  [{JID, Lang}];
                              _ -> []
                          end;
                      _Error -> []
                  end
          end, Watchers),
    lists:foreach(
      fun({JID, Lang}) ->
              Format =
                  echessd_lib:gettext(
                    txt_ply_notification, Lang),
              {Coords, Meta} =
                  case Ply of
                      {_, _} -> Ply;
                      _ -> {Ply, []}
                  end,
              Message =
                  case proplists:get_value(comment, Meta) of
                      [_ | _] = Comment ->
                          io_lib:format(
                            Format ++ "~n" ++
                                echessd_lib:gettext(
                                  txt_ply_notification_comment, Lang),
                            [GameID, Username, Coords, Comment]);
                      _ ->
                          io_lib:format(
                            Format, [GameID, Username, Coords])
                  end,
              Cmd =
                  lists:flatten(
                    io_lib:format(
                      "echo \"~s\" | sendxmpp -u \"~s\" -j \"~s\" "
                      "-p \"~s\" \"~s\"",
                      [escape_quotes(Message),
                       escape_quotes(XmppUser),
                       escape_quotes(XmppServer),
                       escape_quotes(XmppPassword),
                       escape_quotes(JID)])),
              echessd_log:debug("NOTIFYING: ~s", [Cmd]),
              Result = os:cmd(Cmd),
              echessd_log:debug("NOTIFY RESULT: ~s", [Result])
      end, JIDs).

escape_quotes([]) -> [];
escape_quotes([C | Tail]) ->
    case lists:member(C, "\"$`!") of
        true ->
            [$\\, C | escape_quotes(Tail)];
        _ ->
            [C | escape_quotes(Tail)]
    end.

