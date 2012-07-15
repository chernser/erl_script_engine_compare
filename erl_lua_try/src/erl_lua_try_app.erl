-module(erl_lua_try_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	application:start(erl_lua_try).

start(_StartType, _StartArgs) ->
    erl_lua_try_sup:start_link().

stop(_State) ->
    ok.
