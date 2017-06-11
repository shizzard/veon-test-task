-module(veon_web_app).
-behaviour(application).

-include_lib("veon_helper/include/veon_specs_application.hrl").

-export([start/2, stop/1]).


%% Interface


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/movie/store", movie_store_handler, []},
            {"/movie/lookup", movie_lookup_handler, []},
            {"/reserve/add", movie_add_reserve_handler, []},
            {"/reserve/remove", movie_remove_reserve_handler, []},
            {'_', generic_not_found_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 20, [{port, 10080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    veon_web_sup:start_link().


stop(_State) ->
    ok.
