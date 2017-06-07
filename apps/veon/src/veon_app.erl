-module(veon_app).
-behaviour(application).

-include_lib("veon_helper/include/veon_specs_application.hrl").

-export([start/2, stop/1]).


%% Interface 


start(_StartType, _StartArgs) ->
    veon_sup:start_link().


stop(_State) ->
    ok.
