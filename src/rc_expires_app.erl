-module(rc_expires_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rc_expires_sup:start_link().

stop(_State) ->
    ok.
