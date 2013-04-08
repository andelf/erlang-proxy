-module(proxy_client).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    proxy_client_sup:start_link().

stop(_State) ->
    ok.
