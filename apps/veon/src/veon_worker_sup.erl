-module(veon_worker_sup).
-behaviour(supervisor).

-include_lib("veon_helper/include/veon_specs_supervisor.hrl").

-export([start_link/0, init/1]).


%% Interface


-spec start_link() ->
    veon_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
        {
            veon_worker, {veon_worker, start_link, []},
            permanent, 5000, worker, [veon_worker]
        }
    ]}}.

