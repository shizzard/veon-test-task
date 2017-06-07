-module(veon_pdu).

-export([parse_json/1, render_json/1]).


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


%% Behavior callbacks


-callback validate(Document :: jiffy_v:jv_data()) ->
    veon_helper_type:generic_return(
        OkRet :: jiffy_v:jv_ret_result(),
        ErrorRet :: jiffy_v:jv_ret_errorlist()
    ).

-callback from_document(JiffyDocument :: jiffy_v:jv_data()) ->
    veon_helper_type:ok_return(
        OkRet :: term()
    ).

-callback to_document(Record :: term()) ->
    veon_helper_type:ok_return(
        OkRet :: jiffy_v:jv_data()
    ).


%% Interface


-spec parse_json(Json :: binary()) ->
    veon_helper_type:generic_return(
        OkRet :: jiffy_v:jv_data(),
        ErrorRet :: term()
    ).

parse_json(Json) ->
    try jiffy:decode(Json) of
        Document ->
            {ok, Document}
    catch
        throw:E ->
            {error, E}
    end.


-spec render_json(Document :: jiffy_v:jv_data()) ->
    veon_helper_type:generic_return(
        OkRet :: binary(),
        ErrorRet :: term()
    ).

render_json(Document) ->
    try jiffy:encode(Document) of
        Json ->
            {ok, Json}
    catch
        throw:E ->
            {error, E}
    end.
