-module(veon_storage).

-export([
    new_movie/4, new_movie/5,
    imdb_id/1, movie_title/1, available_seats/1, reserved_seats/1, screen_id/1,
    imdb_id/2, movie_title/2, available_seats/2, reserved_seats/2, screen_id/2, reservation_count/1,
    new_reserve/0, new_reserve/1,
    reservation_id/1, reservation_id/2,
    add_reserve/2, remove_reserve/2
]).


%% Types


-type imdb_id() :: binary().
-type movie_title() :: binary().
-type screen_id() :: binary().
-type reservation_id() :: binary().
-type seats_count() :: non_neg_integer().
-type available_seats() :: seats_count().
-type reserved_seats() :: seats_count().

-export_type([
    imdb_id/0, movie_title/0, reservation_id/0, screen_id/0,
    seats_count/0, available_seats/0, reserved_seats/0
]).


-record(veon_storage_movie, {
    key :: {imdb_id(), screen_id()},
    imdb_id :: imdb_id(),
    movie_title :: movie_title(),
    available_seats :: available_seats(),
    reserved_seats :: dict:dict(reservation_id(), veon_storage_reserve()),
    screen_id :: screen_id()
}).
-type veon_storage_movie() :: #veon_storage_movie{}.
-record(veon_storage_reserve, {
    reservation_id :: reservation_id()
}).
-type veon_storage_reserve() :: #veon_storage_reserve{}.
-export_type([veon_storage_movie/0, veon_storage_reserve/0]).


%% Interface


-spec new_movie(
    ImdbId :: imdb_id(),
    MovieTitle :: movie_title(),
    AvailableSeats :: available_seats(),
    ScreenId :: screen_id()
) ->
    Ret :: veon_storage_movie().

new_movie(ImdbId, MovieTitle, AvailableSeats, ScreenId) ->
    #veon_storage_movie{
        key = {ImdbId, ScreenId},
        imdb_id = ImdbId,
        movie_title = MovieTitle,
        available_seats = AvailableSeats,
        reserved_seats = dict:new(),
        screen_id = ScreenId
    }.


-spec new_movie(
    ImdbId :: imdb_id(),
    MovieTitle :: movie_title(),
    AvailableSeats :: available_seats(),
    ReservedSeats :: dict:dict(reservation_id(), veon_storage_reserve()),
    ScreenId :: screen_id()
) ->
    Ret :: veon_storage_movie().

new_movie(ImdbId, MovieTitle, AvailableSeats, ReservedSeats, ScreenId) ->
    #veon_storage_movie{
        key = {ImdbId, ScreenId},
        imdb_id = ImdbId,
        movie_title = MovieTitle,
        available_seats = AvailableSeats,
        reserved_seats = ReservedSeats,
        screen_id = ScreenId
    }.


-spec imdb_id(Movie :: veon_storage_movie()) ->
    Ret :: imdb_id().

imdb_id(#veon_storage_movie{imdb_id = ImdbId}) ->
    ImdbId.


-spec imdb_id(Movie :: veon_storage_movie(), Value :: imdb_id()) ->
    Ret :: veon_storage_movie().

imdb_id(#veon_storage_movie{screen_id = ScreenId} = Record, Value) ->
    Record#veon_storage_movie{key = {Value, ScreenId}, imdb_id = Value}.


-spec movie_title(Movie :: veon_storage_movie()) ->
    Ret :: movie_title().

movie_title(#veon_storage_movie{movie_title = MovieTitle}) ->
    MovieTitle.


-spec movie_title(Movie :: veon_storage_movie(), Value :: movie_title()) ->
    Ret :: veon_storage_movie().

movie_title(#veon_storage_movie{} = Record, Value) ->
    Record#veon_storage_movie{movie_title = Value}.


-spec available_seats(Movie :: veon_storage_movie()) ->
    Ret :: available_seats().

available_seats(#veon_storage_movie{available_seats = AvailableSeats}) ->
    AvailableSeats.


-spec available_seats(Movie :: veon_storage_movie(), Value :: available_seats()) ->
    Ret :: veon_storage_movie().

available_seats(#veon_storage_movie{} = Record, Value) ->
    Record#veon_storage_movie{available_seats = Value}.


-spec reserved_seats(Movie :: veon_storage_movie()) ->
    Ret :: dict:dict(reservation_id(), veon_storage_reserve()).

reserved_seats(#veon_storage_movie{reserved_seats = ReservedSeats}) ->
    ReservedSeats.


-spec reserved_seats(
    Movie :: veon_storage_movie(),
    Value :: dict:dict(reservation_id(), veon_storage_reserve())
) ->
    Ret :: veon_storage_movie().

reserved_seats(#veon_storage_movie{} = Record, Value) ->
    Record#veon_storage_movie{reserved_seats = Value}.


-spec screen_id(Movie :: veon_storage_movie()) ->
    Ret :: screen_id().

screen_id(#veon_storage_movie{screen_id = ScreenId}) ->
    ScreenId.


-spec screen_id(Movie :: veon_storage_movie(), Value :: screen_id()) ->
    Ret :: veon_storage_movie().

screen_id(#veon_storage_movie{imdb_id = ImdbId} = Record, Value) ->
    Record#veon_storage_movie{key = {ImdbId, Value}, screen_id = Value}.


-spec reservation_count(Movie :: veon_storage_movie()) ->
    Ret :: reserved_seats().

reservation_count(Movie) ->
    dict:size(Movie#veon_storage_movie.reserved_seats).


-spec new_reserve() ->
    Ret :: veon_storage_reserve().

new_reserve() ->
    #veon_storage_reserve{reservation_id = list_to_binary(uuid:to_string(uuid:uuid4()))}.


-spec new_reserve(ReservationId :: reservation_id()) ->
    Ret :: veon_storage_reserve().

new_reserve(ReservationId) ->
    #veon_storage_reserve{reservation_id = ReservationId}.


-spec reservation_id(Movie :: veon_storage_reserve()) ->
    Ret :: reservation_id().

reservation_id(#veon_storage_reserve{reservation_id = ReservationId}) ->
    ReservationId.


-spec reservation_id(Movie :: veon_storage_reserve(), Value :: reservation_id()) ->
    Ret :: veon_storage_reserve().

reservation_id(#veon_storage_reserve{} = Record, Value) ->
    Record#veon_storage_reserve{reservation_id = Value}.


-spec add_reserve(
    Movie :: veon_storage_movie(),
    Reserve :: veon_storage_reserve()
) ->
    veon_helper_type:generic_return(
        OkRet :: veon_storage_movie(),
        ErrorRet :: reservations_exceeded | duplicate_reserve
    ).

add_reserve(#veon_storage_movie{} = Movie, #veon_storage_reserve{} = Reserve) ->
    ReservationsCount = dict:size(Movie#veon_storage_movie.reserved_seats),
    SeatsAvailable = Movie#veon_storage_movie.available_seats > ReservationsCount,
    ReservationId = reservation_id(Reserve),
    Reservations = reserved_seats(Movie),
    ReserveAlreadyExists = dict:is_key(ReservationId, Reservations),
    case {SeatsAvailable, ReserveAlreadyExists} of
        {false, _} ->
            {error, reservations_exceeded};
        {_, true} ->
            {error, duplicate_reserve};
        {true, false} ->
            Reservations1 = dict:store(ReservationId, Reserve, Reservations),
            Movie1 = reserved_seats(Movie, Reservations1),
            {ok, Movie1}
    end.


-spec remove_reserve(
    Movie :: veon_storage_movie(),
    Reservation :: reservation_id() | veon_storage_reserve()
) ->
    veon_helper_type:generic_return(
        OkRet :: veon_storage_movie(),
        ErrorRet :: not_found
    ).

remove_reserve(#veon_storage_movie{} = Movie, ReservationId)
when is_binary(ReservationId) ->
    Reservations = reserved_seats(Movie),
    case dict:is_key(ReservationId, Reservations) of
        true ->
            Reservations1 = dict:erase(ReservationId, Reservations),
            {ok, reserved_seats(Movie, Reservations1)};
        false ->
            {error, not_found}
    end;

remove_reserve(#veon_storage_movie{} = Movie, #veon_storage_reserve{} = Reserve) ->
    remove_reserve(Movie, reservation_id(Reserve)).
