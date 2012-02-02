%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 1 Feb 2012
%%% @copyright 2012, Aleksey Morarash
%%% @doc HTTPD warden process

-module(echessd_web_warden).

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
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%% @doc Sends 'reconfig' signal to warden process.
%% @spec reconfig() -> ok
reconfig() ->
    catch ?MODULE ! ?reconfig,
    ok.

%% ----------------------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------------------

-record(state,
        {bindaddr,
         bindport,
         docroot,
         module,
         server
        }).

init(_Args) ->
    process_flag(trap_exit, true),
    self() ! ?reconfig,
    echessd_log:info("~w> started", [?MODULE]),
    {ok, #state{}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(?reconfig, State) ->
    echessd_log:debug("~w> got reconfig signal!", [?MODULE]),
    BindAddr = echessd_cfg:get(?CFG_BIND_ADDR),
    BindPort = echessd_cfg:get(?CFG_BIND_PORT),
    DocRoot  = echessd_cfg:get(?CFG_DOC_ROOT),
    HttpdMod = echessd_cfg:get(?CFG_HTTPD_MOD),
    NeedToRestart =
        not is_pid(State#state.server) orelse
        State#state.bindaddr /= BindAddr orelse
        State#state.bindport /= BindPort orelse
        State#state.docroot /= DocRoot orelse
        State#state.module /= HttpdMod,
    if NeedToRestart ->
            catch exit(State#state.server, kill),
            echessd_log:debug("starting httpd..."),
            try HttpdMod:start_link(
                  BindAddr, BindPort, DocRoot) of
                {ok, Pid} when is_pid(Pid) ->
                    echessd_log:info(
                      "HTTP server (~w) listens at ~s:~w",
                      [HttpdMod, echessd_lib:ip2str(BindAddr),
                       BindPort]),
                    {noreply,
                     State#state{
                       bindaddr = BindAddr,
                       bindport = BindPort,
                       docroot  = DocRoot,
                       module   = HttpdMod,
                       server   = Pid
                      }};
                Other ->
                    echessd_log:err(
                      "HTTP server start failed: "
                      "bad mod (~w) return: ~99999p",
                      [HttpdMod, Other]),
                    throw({bad_return, HttpdMod, Other})
            catch
                Type:Reason ->
                    FinalReason =
                        {Type, Reason, erlang:get_stacktrace()},
                    echessd_log:err(
                      "HTTP server start failed. "
                      "Args: ~99999p. "
                      "Reason: ~99999p.",
                      [[{module, HttpdMod},
                        {bindaddr, BindAddr},
                        {bindport, BindPort},
                        {docroot, DocRoot}],
                       FinalReason]),
                    throw(FinalReason)
            end;
       true ->
            {noreply, State}
    end;
handle_info({'EXIT', From, Reason}, State)
  when From == State#state.server ->
    echessd_log:err(
      "HTTPD process (mod ~w) died: ~99999p",
      [State#state.module, Reason]),
    self() ! ?reconfig,
    {noreply, State#state{server = undefined}};
handle_info(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    echessd_log:info(
      "~w> terminating due to: ~99999p",
      [?MODULE, Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

