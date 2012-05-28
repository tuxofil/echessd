%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 18 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc gameplay notifications tools.

-module(echessd_notify).

-export(
   [ply/3,
    game_add/1,
    game_ack/1,
    game_end/1
   ]).

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
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Watchers =
        lists:usort(
          echessd_game:get_watchers(GameInfo)),
    do_notifies(
      fun(Lang) ->
              Format = gettext(txt_ply_notification, Lang),
              {Coords, Meta} =
                  case Ply of
                      {_, _} -> Ply;
                      _ -> {Ply, []}
                  end,
              FinalCoords =
                  case proplists:get_value(notation, Meta) of
                      [_ | _] = Notation -> Notation;
                      _ -> Coords
                  end,
              case proplists:get_value(comment, Meta) of
                  [_ | _] = Comment ->
                      io_lib:format(
                        Format ++ "~n" ++
                            gettext(txt_ply_notification_comment, Lang),
                        [GameID, Username, FinalCoords, Comment]);
                  _ ->
                      io_lib:format(
                        Format, [GameID, Username, FinalCoords])
              end
      end, Watchers -- [Username]).

%% @doc Dispatches notifications about game creation.
%% @spec game_add(GameID) -> ok
%%     GameID = echessd_game:echessd_game_id()
game_add(GameID) ->
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Creator = echessd_game:get_creator(GameInfo),
    Watchers =
        lists:usort(
          echessd_game:get_watchers(GameInfo)),
    Color = echessd_game:get_player_color(GameInfo, Creator),
    do_notifies(
      fun(Lang) ->
              io_lib:format(
                gettext(txt_game_add_notification, Lang),
                [Creator, localize_color(Color, Lang)])
      end, Watchers -- [Creator]).

%% @doc Dispatches notifications about game acknowledge.
%% @spec game_ack(GameID) -> ok
%%     GameID = echessd_game:echessd_game_id()
game_ack(GameID) ->
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Creator = echessd_game:get_creator(GameInfo),
    CreatorColor = echessd_game:get_player_color(GameInfo, Creator),
    Opponent = echessd_game:get_opponent(GameInfo, Creator),
    OpponentColor = echessd_game:get_player_color(GameInfo, Opponent),
    Watchers =
        lists:usort(
          echessd_game:get_watchers(GameInfo)),
    do_notifies(
      fun(Lang) ->
              io_lib:format(
                gettext(txt_game_ack_notification, Lang),
                [Creator, localize_color(CreatorColor, Lang),
                 Opponent, localize_color(OpponentColor, Lang)])
      end, Watchers -- [Opponent]).

%% @doc Dispatches notifications about game end.
%% @spec game_end(GameID) -> ok
%%     GameID = echessd_game:echessd_game_id()
game_end(GameID) ->
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Watchers =
        lists:usort(
          echessd_game:get_watchers(GameInfo)),
    case proplists:get_value(status, GameInfo) of
        none -> ok;
        give_up ->
            Winner =
                proplists:get_value(winner, GameInfo),
            WinnerColor =
                proplists:get_value(winner_color, GameInfo),
            Looser =
                echessd_game:get_opponent(GameInfo, Winner),
            LooserColor =
                echessd_game:get_player_color(GameInfo, Looser),
            do_notifies(
              fun(Lang) ->
                      io_lib:format(
                        gettext(txt_give_up_notification, Lang),
                        [GameID,
                         Winner, localize_color(WinnerColor, Lang),
                         Looser, localize_color(LooserColor, Lang)])
              end, Watchers -- [Looser]);
        checkmate ->
            Winner =
                proplists:get_value(winner, GameInfo),
            WinnerColor =
                proplists:get_value(winner_color, GameInfo),
            Looser =
                echessd_game:get_opponent(GameInfo, Winner),
            LooserColor =
                echessd_game:get_player_color(GameInfo, Looser),
            do_notifies(
              fun(Lang) ->
                      io_lib:format(
                        gettext(txt_checkmate_notification, Lang),
                        [GameID,
                         Winner, localize_color(WinnerColor, Lang),
                         Looser, localize_color(LooserColor, Lang)])
              end, Watchers -- [Winner]);
        {draw, _} ->
            Creator =
                echessd_game:get_creator(GameInfo),
            CreatorColor =
                echessd_game:get_player_color(GameInfo, Creator),
            Opponent =
                echessd_game:get_opponent(GameInfo, Creator),
            OpponentColor =
                echessd_game:get_player_color(GameInfo, Opponent),
            do_notifies(
              fun(Lang) ->
                      io_lib:format(
                        gettext(txt_draw_notification, Lang),
                        [GameID,
                         Creator, localize_color(CreatorColor, Lang),
                         Opponent, localize_color(OpponentColor, Lang)])
              end, Watchers)
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

do_notifies(MessageGenerator, Usernames) ->
    lists:foreach(
      fun(Username) ->
              do_notify(MessageGenerator, Username)
      end, Usernames).

do_notify(MessageGenerator, Username) ->
    spawn(
      fun() ->
              do_notify_(MessageGenerator, Username)
      end), ok.
do_notify_(MessageGenerator, Username) ->
    case fetch_xmpp_requisites() of
        {ok, XmppUser, XmppServer, XmppPassword} ->
            case echessd_user:getprops(Username) of
                {ok, UserInfo} ->
                    NotifyEnabled = echessd_user:get_value(notify, UserInfo),
                    case proplists:get_value(jid, UserInfo) of
                        [_ | _] = JabberID when NotifyEnabled ->
                            Lang = echessd_user:get_value(language, UserInfo),
                            try MessageGenerator(Lang) of
                                [] -> nop;
                                [_ | _] = Message ->
                                    do_notify(
                                      JabberID, Message,
                                      XmppUser, XmppServer, XmppPassword);
                                Other ->
                                    echessd_log:error(
                                      "~w: failed to format notification - "
                                      "bad string: ~99999p",
                                      [?MODULE, Other])
                            catch
                                Type:Reason ->
                                    echessd_log:error(
                                      "~w: failed to format notification: ~9999p",
                                      [?MODULE,
                                       {Type, Reason, erlang:get_stacktrace()}])
                            end;
                        _ -> ok
                    end;
                {error, Reason} ->
                    echessd_log:error(
                      "~w: failed to fetch info for \"~s\": ~9999p",
                      [?MODULE, Username, Reason]),
                    ok
            end;
        _ -> ok
    end.

do_notify(JID, Message, XmppUser, XmppServer, XmppPassword) ->
    try
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
        echessd_log:debug("NOTIFY RESULT: ~s", [Result]),
        ok
    catch
        Type:Reason ->
            echessd_log:error(
              "~w: failed to format notify cmd: ~99999p",
              [?MODULE, {Type, Reason, erlang:get_stacktrace()}]),
            ok
    end.

escape_quotes([]) -> [];
escape_quotes([C | Tail]) ->
    case lists:member(C, "\"$`!") of
        true ->
            [$\\, C | escape_quotes(Tail)];
        _ ->
            [C | escape_quotes(Tail)]
    end.

gettext(StrID, Lang) -> echessd_lib:gettext(StrID, Lang).

localize_color(?white, Lang) ->
    gettext(txt_notif_color_white, Lang);
localize_color(?black, Lang) ->
    gettext(txt_notif_color_black, Lang);
localize_color(_, _) -> "".

%% @doc Fetches XMPP account requisites, if permitted.
%% @spec fetch_xmpp_requisites() ->
%%         {ok, XmppUser, XmppServer, XmppPassword} | undefined
%%     XmppUser = string(),
%%     XmppServer = string(),
%%     XmppPassword = string()
fetch_xmpp_requisites() ->
    case echessd_cfg:get(?CFG_XMPP_ENABLED) of
        true ->
            XmppUser = echessd_cfg:get(?CFG_XMPP_USER),
            XmppServer = echessd_cfg:get(?CFG_XMPP_SERVER),
            case {XmppUser, XmppServer} of
                {[_ | _], [_ | _]} ->
                    XmppPassword = echessd_cfg:get(?CFG_XMPP_PASSWORD),
                    {ok, XmppUser, XmppServer, XmppPassword};
                _ -> undefined
            end;
        _ -> undefined
    end.

