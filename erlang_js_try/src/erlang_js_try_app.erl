-module(erlang_js_try_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	application:start(erlang_js_try).

start(_StartType, _StartArgs) ->
    erlang_js_try_sup:start_link().

stop(_State) ->
    ok.
