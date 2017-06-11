-module(veon_pdu_movie_reserve_remove_req).
-behavior(veon_pdu).

-export([
    new/3, imdb_id/1, screen_id/1, reservation_id/1,
    imdb_id/2, screen_id/2, reservation_id/2,
    from_document/1, to_document/1, validate/1
]).

-record(?MODULE, {
    imdb_id :: veon_storage:imdb_id(),
    screen_id :: veon_storage:screen_id(),
    reservation_id :: veon_storage:reservation_id()
}).
-type pdu() :: #?MODULE{}.
-export_type([pdu/0]).

-include("datafield_definitions.hrl").


%% Interface


-spec new(
    ImbdId :: veon_storage:imdb_id(),
    ScreenId :: veon_storage:screen_id(),
    ReservationId :: veon_storage:reservation_id()
) ->
    pdu().

new(ImbdId, ScreenId, ReservationId) ->
    #?MODULE{
        imdb_id = ImbdId, screen_id = ScreenId, reservation_id = ReservationId
    }.


-spec imdb_id(Record :: pdu()) ->
    Ret :: veon_storage:imdb_id().

imdb_id(#?MODULE{imdb_id = ImdbId}) ->
    ImdbId.


-spec imdb_id(Record :: pdu(), Value :: veon_storage:imdb_id()) ->
    Ret :: pdu().

imdb_id(#?MODULE{} = Record, Value) ->
    Record#?MODULE{imdb_id = Value}.


-spec screen_id(Record :: pdu()) ->
    Ret :: veon_storage:screen_id().

screen_id(#?MODULE{screen_id = ScreenId}) ->
    ScreenId.


-spec screen_id(Record :: pdu(), Value :: veon_storage:screen_id()) ->
    Ret :: pdu().

screen_id(#?MODULE{} = Record, Value) ->
    Record#?MODULE{screen_id = Value}.


-spec reservation_id(Record :: pdu()) ->
    Ret :: veon_storage:reservation_id().

reservation_id(#?MODULE{reservation_id = ReservationId}) ->
    ReservationId.


-spec reservation_id(Record :: pdu(), Value :: veon_storage:reservation_id()) ->
    Ret :: pdu().

reservation_id(#?MODULE{} = Record, Value) ->
    Record#?MODULE{reservation_id = Value}.


-spec validate(Document :: jiffy_v:jv_data()) ->
    veon_helper_type:generic_return(
        OkRet :: jiffy_v:jv_ret_result(),
        ErrorRet :: jiffy_v:jv_ret_errorlist()
    ).

validate(Document) ->
    case jiffy_v:validate(validate_map(), Document, fun validate_fun/3) of
        {[], Result} ->
            {ok, Result};
        {Errors, _} ->
            {error, Errors}
    end.


-spec from_document(Document :: jiffy_v:jv_data()) ->
    veon_helper_type:ok_return(
        OkRet :: pdu()
    ).

from_document(Document) ->
    {ok, new(
        veon_helper_jiffy:unpack(veon_helper_jiffy:get([?datafield_imdb_id], Document)),
        veon_helper_jiffy:unpack(veon_helper_jiffy:get([?datafield_screen_id], Document)),
        veon_helper_jiffy:unpack(veon_helper_jiffy:get([?datafield_reservation_id], Document))
    )}.


-spec to_document(Record :: pdu()) ->
    veon_helper_type:ok_return(
        OkRet :: jiffy_v:jv_data()
    ).

to_document(Record) ->
    {ok, {[
        {?datafield_imdb_id, imdb_id(Record)},
        {?datafield_screen_id, screen_id(Record)},
        {?datafield_reservation_id, reservation_id(Record)}
    ]}}.


%% Internals


-spec validate_map() ->
    Ret :: jiffy_v:jv_type().

validate_map() ->
    {hash, [
        {?datafield_imdb_id, required, {string}},
        {?datafield_screen_id, required, {string}},
        {?datafield_reservation_id, required, {string}}
    ]}.


-spec validate_fun(
    Type :: validate | fix,
    Stack :: jiffy_v:jv_ret_stack(),
    Value :: jiffy_v:jv_data()
) ->
    {ok, valid} | {ok, NewValue :: jiffy_v:jv_data()} | {error, Code :: jiffy_v:jv_ret_code()} |
    {ok, NewValue :: jiffy_v:jv_data()} | {error, invalid} | {error, Code :: jiffy_v:jv_ret_code()}.

validate_fun(validate, Stack, Value) ->
    validate_fun_validate(Stack, Value);

validate_fun(fix, Stack, Value) ->
    validate_fun_fix(Stack, Value).


-spec validate_fun_validate(Stack :: jiffy_v:jv_ret_stack(), Value :: jiffy_v:jv_data()) ->
    Ret :: {ok, valid} | {ok, NewValue :: jiffy_v:jv_data()} | {error, Code :: jiffy_v:jv_ret_code()}.

validate_fun_validate([?datafield_imdb_id], Value)
when byte_size(Value) > 128 ->
    {error, <<"IMDB ID is too long">>};

validate_fun_validate([?datafield_screen_id], Value)
when byte_size(Value) > 128 ->
    {error, <<"Screen ID is too long">>};

validate_fun_validate([?datafield_reservation_id], Value)
when byte_size(Value) > 128 ->
    {error, <<"Reservation ID is too long">>};

validate_fun_validate(_Stack, _Value) ->
    {ok, valid}.


-spec validate_fun_fix(Stack :: jiffy_v:jv_ret_stack(), Value :: jiffy_v:jv_data()) ->
    Ret :: {ok, NewValue :: jiffy_v:jv_data()} | {error, invalid} | {error, Code :: jiffy_v:jv_ret_code()}.

validate_fun_fix([?datafield_imdb_id], _Value) ->
    {error, <<"Invalid IMDB ID">>};

validate_fun_fix([?datafield_screen_id], _Value) ->
    {error, <<"Invalid screen ID">>};

validate_fun_fix([?datafield_reservation_id], _Value) ->
    {error, <<"Invalid reservation ID">>};

validate_fun_fix(_Stack, _Value) ->
    {error, invalid}.
