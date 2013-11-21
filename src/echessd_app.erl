%%% @doc
%%% Erlang application behaviour module.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Jan 2012
%%% @copyright 2012, Aleksey Morarash

-module(echessd_app).

-behaviour(application).

-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

-include("echessd.hrl").

%% ----------------------------------------------------------------------
%% API: 'application' behaviour callback functions
%% ----------------------------------------------------------------------

%% @hidden
-spec start(StartType :: application:start_type(),
            StartArgs :: any()) ->
                   {ok, Pid :: pid()} | {error, Reason :: any()}.
start(_StartType, _StartArgs) ->
    echessd_sup:start_link().

%% @hidden
-spec start_phase(Phase :: atom(), StartType :: application:start_type(),
                  PhaseArgs :: any()) -> ok.
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%% @hidden
-spec prep_stop(State :: any()) -> ok.
prep_stop(_State) ->
    ok.

%% @hidden
-spec stop(State :: any()) -> ok.
stop(_State) ->
    ok.

%% @hidden
-spec config_change(Changed :: [{Key :: atom(), Value :: any()}],
                    New :: [{Key :: atom(), Value :: any()}],
                    Removed :: [Key :: atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

