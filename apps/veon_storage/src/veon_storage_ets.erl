-module(veon_storage_ets).
-behavior(veon_storage).

-export([new/0, lookup/3, store/2]).

-record(?MODULE, {
    ets :: ets:tid()
}).
-opaque ?MODULE() :: #?MODULE{}.
-export_type([?MODULE/0]).


%% Internals


-spec new() ->
    veon_helper_type:ok_return(OkRet :: ?MODULE()).

new() ->
    Ets = ets:new(?MODULE, [
        ordered_set, protected,
        {keypos, 2},
        {read_concurrency, true}
    ]),
    {ok, #?MODULE{ets = Ets}}.


-spec lookup(
    Storage :: ?MODULE(),
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id()
) ->
    veon_helper_type:generic_return(
        OkRet :: veon_storage:veon_storage_movie(),
        ErrorRet :: not_found
    ).

lookup(#?MODULE{} = Storage, ImdbId, ScreenId) ->
    Key = veon_storage:key(ImdbId, ScreenId),
    case ets:lookup(Storage#?MODULE.ets, Key) of
        [Movie] ->
            {ok, Movie};
        [] ->
            {error, not_found}
    end.


-spec store(Storage :: ?MODULE(), Movie :: veon_storage:veon_storage_movie()) ->
    veon_helper_type:generic_return(
        ErrorRet :: term()
    ).

store(#?MODULE{} = Storage, Movie) ->
    ets:insert(Storage#?MODULE.ets, Movie),
    ok.
