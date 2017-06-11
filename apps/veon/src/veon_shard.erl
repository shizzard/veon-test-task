-module(veon_shard).

-export([id_for/1, worker_by_id/1]).

-type shard_id() :: non_neg_integer().
-type worker_id() :: atom() | pid().
-export_type([shard_id/0, worker_id/0]).

%% Interface


-spec id_for(In :: term()) ->
    Ret :: shard_id().

id_for(Binary) when is_binary(Binary) ->
    erlang:crc32(Binary) rem veon_config:worker_count();

id_for(Term) ->
    id_for(erlang:term_to_binary(Term)).


-spec worker_by_id(Id :: shard_id()) ->
    Ret :: worker_id().

worker_by_id(0) ->  'veon_worker-0';
worker_by_id(1) ->  'veon_worker-1';
worker_by_id(2) ->  'veon_worker-2';
worker_by_id(3) ->  'veon_worker-3';
worker_by_id(4) ->  'veon_worker-4';
worker_by_id(5) ->  'veon_worker-5';
worker_by_id(6) ->  'veon_worker-6';
worker_by_id(7) ->  'veon_worker-7';
worker_by_id(8) ->  'veon_worker-8';
worker_by_id(9) ->  'veon_worker-9';
worker_by_id(10) -> 'veon_worker-10';
worker_by_id(11) -> 'veon_worker-11';
worker_by_id(12) -> 'veon_worker-12';
worker_by_id(13) -> 'veon_worker-13';
worker_by_id(14) -> 'veon_worker-14';
worker_by_id(15) -> 'veon_worker-15';
worker_by_id(_) ->  error(out_of_bounds).
