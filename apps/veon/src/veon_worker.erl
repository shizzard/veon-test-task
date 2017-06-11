-module(veon_worker).
-behaviour(gen_server).

-export([store_movie/5, lookup_movie/3, add_reserve/3, remove_reserve/4]).
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("veon_helper/include/veon_specs_gen_server.hrl").

-define(storage_mod, veon_storage_ets).
-define(
    store_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId),
    {store_movie, ImdbId, MovieTitle, AvailableSeats, ScreenId}
).
-define(
    lookup_movie(ImdbId, MovieTitle),
    {lookup_movie, ImdbId, MovieTitle}
).
-define(
    add_reserve(ImdbId, MovieTitle),
    {add_reserve, ImdbId, MovieTitle}
).
-define(
    remove_reserve(ImdbId, MovieTitle, ReservationId),
    {remove_reserve, ImdbId, MovieTitle, ReservationId}
).

-record(state, {
    storage :: ?storage_mod:?storage_mod()
}).
-type state() :: #state{}.


%% Interface


-spec store_movie(
    Worker :: veon_shard:worker_id(),
    ImdbId :: veon_storage:imdb_id(),
    MovieTitle :: veon_storage:movie_title(),
    AvailableSeats :: veon_storage:available_seats(),
    ScreenId :: veon_storage:screen_id()
) ->
    veon_helper_type:generic_return(
        OkRet :: veon_storage:veon_storage_movie(),
        ErrorRet :: atom()
    ).

store_movie(Worker, ImdbId, MovieTitle, AvailableSeats, ScreenId) ->
    gen_server:call(Worker, ?store_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId)).


-spec lookup_movie(
    Worker :: veon_shard:worker_id(),
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id()
) ->
    veon_helper_type:generic_return(
        OkRet :: veon_storage:veon_storage_movie(),
        ErrorRet :: not_found | atom()
    ).

lookup_movie(Worker, ImdbId, ScreenId) ->
    gen_server:call(Worker, ?lookup_movie(ImdbId, ScreenId)).


-spec add_reserve(
    Worker :: veon_shard:worker_id(),
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id()
) ->
    veon_helper_type:generic_return(
        OkRet :: {veon_storage:veon_storage_movie(), veon_storage:veon_storage_reserve()},
        ErrorRet :: not_found | reservations_exceeded | atom()
    ).

add_reserve(Worker, ImdbId, ScreenId) ->
    gen_server:call(Worker, ?add_reserve(ImdbId, ScreenId)).


-spec remove_reserve(
    Worker :: veon_shard:worker_id(),
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id(),
    ReservationId :: veon_storage:reservation_id()
) ->
    veon_helper_type:generic_return(
        OkRet :: {veon_storage:veon_storage_movie()},
        ErrorRet :: not_found | atom()
    ).

remove_reserve(Worker, ImdbId, ScreenId, ReservationId) ->
    gen_server:call(Worker, ?remove_reserve(ImdbId, ScreenId, ReservationId)).


-spec start_link(Id :: atom()) ->
    veon_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | term()
    ) | ignore.

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, init, []).


init(_) ->
    {ok, Storage} = ?storage_mod:new(),
    {ok, #state{
        storage = Storage
    }}.


handle_call(?store_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId), _GenReplyTo, State) ->
    handle_call_store_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId, State);

handle_call(?lookup_movie(ImdbId, ScreenId), _GenReplyTo, State) ->
    handle_call_lookup_movie(ImdbId, ScreenId, State);

handle_call(?add_reserve(ImdbId, ScreenId), _GenReplyTo, State) ->
    handle_call_add_reserve(ImdbId, ScreenId, State);

handle_call(?remove_reserve(ImdbId, ScreenId, ReservationId), _GenReplyTo, State) ->
    handle_call_remove_reserve(ImdbId, ScreenId, ReservationId, State);

handle_call(Unexpected, _GenReplyTo, State) ->
    lager:error("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, State}.


handle_cast(Unexpected, State) ->
    lager:error("Unexpected cast: ~p", [Unexpected]),
    {noreply, State}.


handle_info(Unexpected, State) ->
    lager:error("Unexpected info: ~p", [Unexpected]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internals


-spec handle_call_store_movie(
    ImdbId :: veon_storage:imdb_id(),
    MovieTitle :: veon_storage:movie_title(),
    AvailableSeats :: veon_storage:available_seats(),
    ScreenId :: veon_storage:screen_id(),
    State :: state()
) ->
    veon_helper_type:gen_server_reply_simple(
        Reply :: veon_helper_type:ok_return(
            OkRet :: veon_storage:veon_storage_movie()
        ),
        State :: state()
    ).

handle_call_store_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId, State) ->
    Movie = veon_storage:new_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId),
    ok = ?storage_mod:store(State#state.storage, Movie),
    {reply, {ok, Movie}, State}.


-spec handle_call_lookup_movie(
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id(),
    State :: state()
) ->
    veon_helper_type:gen_server_reply_simple(
        Reply :: veon_helper_type:ok_return(
            OkRet :: veon_storage:veon_storage_movie()
        ),
        State :: state()
    ).

handle_call_lookup_movie(ImdbId, ScreenId, State) ->
    case ?storage_mod:lookup(State#state.storage, ImdbId, ScreenId) of
        {ok, Movie} ->
            {reply, {ok, Movie}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end.


-spec handle_call_add_reserve(
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id(),
    State :: state()
) ->
    veon_helper_type:gen_server_reply_simple(
        Reply :: veon_helper_type:generic_return(
            OkRet :: {veon_storage:veon_storage_movie(), veon_storage:veon_storage_reserve()},
            ErrorRet :: not_found | reservations_exceeded
        ),
        State :: state()
    ).

handle_call_add_reserve(ImdbId, ScreenId, State) ->
    case ?storage_mod:lookup(State#state.storage, ImdbId, ScreenId) of
        {ok, Movie} ->
            handle_call_add_reserve(Movie, State);
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end.


-spec handle_call_add_reserve(
    Movie :: veon_storage:veon_storage_movie(),
    State :: state()
) ->
    veon_helper_type:gen_server_reply_simple(
        Reply :: veon_helper_type:generic_return(
            OkRet :: {veon_storage:veon_storage_movie(), veon_storage:veon_storage_reserve()},
            ErrorRet :: reservations_exceeded
        ),
        State :: state()
    ).

handle_call_add_reserve(Movie, State) ->
    Reserve = veon_storage:new_reserve(),
    case veon_storage:add_reserve(Movie, Reserve) of
        {ok, Movie1} ->
            ok = ?storage_mod:store(State#state.storage, Movie1),
            {reply, {ok, {Movie1, Reserve}}, State};
        {error, reservations_exceeded} ->
            {reply, {error, reservations_exceeded}, State};
        {error, duplicate_reserve} ->
            %% Should never happen, huh?
            %% Well, in this case we'll retry with different reservation_id.
            handle_call_add_reserve(Movie, State)
    end.


-spec handle_call_remove_reserve(
    ImdbId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id(),
    ReservationId :: veon_storage:reservation_id(),
    State :: state()
) ->
    veon_helper_type:gen_server_reply_simple(
        Reply :: veon_helper_type:generic_return(
            OkRet :: veon_storage:veon_storage_movie(),
            ErrorRet :: not_found
        ),
        State :: state()
    ).

handle_call_remove_reserve(ImdbId, ScreenId, ReservationId, State) ->
    case ?storage_mod:lookup(State#state.storage, ImdbId, ScreenId) of
        {ok, Movie} ->
            handle_call_remove_reserve(Movie, ReservationId, State);
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end.


-spec handle_call_remove_reserve(
    Movie :: veon_storage:veon_storage_movie(),
    ReservationId :: veon_storage:reservation_id(),
    State :: state()
) ->
    veon_helper_type:gen_server_reply_simple(
        Reply :: veon_helper_type:generic_return(
            OkRet :: veon_storage:veon_storage_movie(),
            ErrorRet :: not_found
        ),
        State :: state()
    ).

handle_call_remove_reserve(Movie, ReservationId, State) ->
    case veon_storage:remove_reserve(Movie, ReservationId) of
        {ok, Movie1} ->
            ok = ?storage_mod:store(State#state.storage, Movie1),
            {reply, {ok, Movie1}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end.
