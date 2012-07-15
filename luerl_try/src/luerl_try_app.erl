-module(luerl_try_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	application:start(luerl_try).


start(_StartType, _StartArgs) ->
    luerl_try_sup:start_link().

stop(_State) ->
    ok.
