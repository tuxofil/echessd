%%% @doc
%%% HTTPD warden process

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 1 Feb 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_httpd_warden).

-behaviour(gen_server).

%% API exports
-export([start_link/0, reconfig/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("echessd.hrl").

-define(reconfig, '*reconfig').

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts warden process as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends 'reconfig' signal to the warden process.
-spec reconfig() -> ok.
reconfig() ->
    catch ?MODULE ! ?reconfig,
    ok.

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state,
        {bindaddr :: inet:ip_address(),
         bindport :: inet:port_number(),
         server :: pid() | undefined
        }).

%% @hidden
-spec init(Args :: any()) -> {ok, InitialState :: #state{}}.
init(_Args) ->
    false = process_flag(trap_exit, true),
    self() ! ?reconfig,
    echessd_log:info("~w> started", [?MODULE]),
    {ok, #state{}}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_info(Info :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(?reconfig, State) ->
    echessd_log:debug("~w> got reconfig signal!", [?MODULE]),
    BindAddr = echessd_cfg:get(?CFG_BIND_ADDR),
    BindPort = echessd_cfg:get(?CFG_BIND_PORT),
    NeedToRestart =
        not is_pid(State#state.server) orelse
        State#state.bindaddr /= BindAddr orelse
        State#state.bindport /= BindPort,
    if NeedToRestart ->
            catch exit(State#state.server, kill),
            echessd_log:debug("starting httpd..."),
            case echessd_httpd:start_link(BindAddr, BindPort) of
                {ok, Pid} when is_pid(Pid) ->
                    echessd_log:info(
                      "HTTP server listens at ~s:~w",
                      [echessd_lib:ip2str(BindAddr), BindPort]),
                    {noreply,
                     State#state{
                       bindaddr = BindAddr,
                       bindport = BindPort,
                       server   = Pid
                      }};
                {error, Reason} ->
                    echessd_log:err(
                      "HTTP server start failed: ~99999p", [Reason]),
                    throw({httpd_start, Reason})
            end;
       true ->
            {noreply, State}
    end;
handle_info({'EXIT', From, Reason}, State)
  when From == State#state.server ->
    echessd_log:err("HTTPD process died: ~99999p", [Reason]),
    self() ! ?reconfig,
    {noreply, State#state{server = undefined}};
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(Reason, _State) ->
    echessd_log:info("~w> terminating due to: ~99999p", [?MODULE, Reason]).

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

