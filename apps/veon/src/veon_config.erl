-module(veon_config).

-export([worker_count/0]).


%% Interface


-spec worker_count() ->
    non_neg_integer().

worker_count() ->
    8.

