-module(veon_app).
-behaviour(application).

-include_lib("veon_helper/include/veon_specs_application.hrl").

-export([start/2, stop/1]).


%% Interface


start(_StartType, _StartArgs) ->
    SupRet = veon_sup:start_link(),
    start_workers(),
    SupRet.


stop(_State) ->
    ok.


%% Internals


-spec start_workers() ->
    veon_helper_type:ok_return().

start_workers() ->
    _OkPids = [
        supervisor:start_child(veon_worker_sup, [veon_shard:worker_by_id(Id)]) ||
        Id <- lists:seq(0, veon_config:worker_count() - 1)
    ],
    ok.
