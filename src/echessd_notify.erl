%%% @doc
%%% Gameplay notification tools.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 18 Feb 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_notify).

-export(
   [game_add/1,
    game_ack/1,
    game_move/3,
    game_end/1
   ]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------

-type message_generator() :: any().

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Dispatch notifications for the move.
-spec game_move(GameID :: echessd_game:id(),
                Username :: echessd_user:name(),
                Ply :: echessd_game:ply()) -> ok.
game_move(GameID, Username, Ply) ->
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

%% @doc Dispatch notifications for a game creation.
-spec game_add(GameID :: echessd_game:id()) -> ok.
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

%% @doc Dispatch notifications for a game acknowledge.
-spec game_ack(GameID :: echessd_game:id()) -> ok.
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

%% @doc Dispatch notifications for the game end.
-spec game_end(GameID :: echessd_game:id()) -> ok.
game_end(GameID) ->
    {ok, GameInfo} = echessd_game:getprops(GameID),
    Watchers =
        lists:usort(
          echessd_game:get_watchers(GameInfo)),
    case proplists:get_value(status, GameInfo) of
        alive -> ok;
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
        Draw when Draw == draw_stalemate; Draw == draw_agreement ->
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

%% @doc
-spec do_notifies(MessageGenerator :: message_generator(),
                  Usernames :: [echessd_user:name()]) -> ok.
do_notifies(MessageGenerator, Usernames) ->
    lists:foreach(
      fun(Username) ->
              do_notify(MessageGenerator, Username)
      end, Usernames).

%% @doc
-spec do_notify(MessageGenerator :: message_generator(),
                Username :: echessd_user:name()) -> ok.
do_notify(MessageGenerator, Username) ->
    spawn(
      fun() ->
              do_notify_(MessageGenerator, Username)
      end), ok.

%% @doc
-spec do_notify_(MessageGenerator :: message_generator(),
                 Username :: echessd_user:name()) -> ok.
do_notify_(MessageGenerator, Username) ->
    case fetch_xmpp_requisites() of
        {ok, XmppUser, XmppServer, XmppPassword} ->
            case echessd_user:getprops(Username) of
                {ok, UserInfo} ->
                    NotifyEnabled = echessd_user:get_value(notify, UserInfo),
                    case echessd_user:get_value(jid, UserInfo) of
                        [_ | _] = JabberID when NotifyEnabled ->
                            Lang = echessd_user:get_value(language, UserInfo),
                            try MessageGenerator(Lang) of
                                [] -> nop;
                                [_ | _] = Message ->
                                    do_notify(
                                      JabberID, Message,
                                      XmppUser, XmppServer, XmppPassword)
                            catch
                                Type:Reason ->
                                    echessd_log:err(
                                      "~w: failed to format notification: ~9999p",
                                      [?MODULE,
                                       {Type, Reason, erlang:get_stacktrace()}])
                            end;
                        _ -> ok
                    end;
                {error, Reason} ->
                    echessd_log:err(
                      "~w: failed to fetch info for \"~s\": ~9999p",
                      [?MODULE, Username, Reason]),
                    ok
            end;
        _ -> ok
    end.

%% @doc
-spec do_notify(JID :: nonempty_string(), Message :: nonempty_string(),
                XmppUser :: nonempty_string(),
                XmppServer :: nonempty_string(),
                XmppPassword :: string()) -> ok.
do_notify(JID, Message, XmppUser, XmppServer, XmppPassword) ->
    StdoutData =
        os:cmd(
          io_lib:format(
            "echo \"~s\" | sendxmpp -u \"~s\" -j \"~s\" -p \"~s\" \"~s\"",
            [escape_quotes(Message),
             escape_quotes(XmppUser),
             escape_quotes(XmppServer),
             escape_quotes(XmppPassword),
             escape_quotes(JID)])),
    echessd_log:debug("NOTIFY RESULT: ~s", [StdoutData]).

%% @doc
-spec escape_quotes(String :: string()) -> Escaped :: string().
escape_quotes(String) ->
    lists:flatmap(
      fun(Char) ->
              case lists:member(Char, [$", $$, $`, $!]) of
                  true ->
                      [$\\, Char];
                  false ->
                      [Char]
              end
      end, String).

%% @doc
-spec gettext(TextID :: atom(), LangID :: atom()) -> string().
gettext(TextID, LangID) ->
    echessd_lang:gettext(TextID, LangID).

%% @doc
-spec localize_color(Color :: echessd_game:color(), LangID :: atom()) ->
                            string().
localize_color(?white, LangID) ->
    gettext(txt_notif_color_white, LangID);
localize_color(?black, LangID) ->
    gettext(txt_notif_color_black, LangID).

%% @doc Fetch XMPP account requisites, if permitted.
-spec fetch_xmpp_requisites() -> {ok,
                                  XmppUser :: nonempty_string(),
                                  XmppServer :: nonempty_string(),
                                  XmppPassword :: string()} |
                                 undefined.
fetch_xmpp_requisites() ->
    case {echessd_cfg:get(?CFG_XMPP_ENABLED),
          echessd_cfg:get(?CFG_XMPP_USER),
          echessd_cfg:get(?CFG_XMPP_SERVER),
          echessd_cfg:get(?CFG_XMPP_PASSWORD)} of
        {true, [_ | _] = User, [_ | _] = Server, Password} ->
            {ok, User, Server, Password};
        _ ->
            undefined
    end.
