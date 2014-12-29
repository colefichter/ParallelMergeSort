-module(parallel_merge_sort_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    parallel_merge_sort_sup:start_link().

stop(_State) ->
    ok.
