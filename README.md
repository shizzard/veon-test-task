# Veon test assignment

Made in about 24 hours (3 days in fulltime means).

## TL;DR

Tested on **Erlang 17.5**.

To build the release run

    shizz@df-worktop:/code/erlang/veon-test-task-clean [master [1c67f44]]
     > make
    ...
    ===> release successfully created!

To start release console run

    shizz@df-worktop:/code/erlang/veon-test-task-clean [master [1c67f44]]
     > make run-release
    ...
    Eshell V6.4  (abort with ^G)
    (veon@df-worktop)1>

To run simple tests

    shizz@df-worktop:/code/erlang/veon-test-task-clean [master [1c67f44]]
     > make run-tests
    ...
    OK

## Solution description

This application consists of five erlang apps:

* `veon_helper` application consists of some helper stuff like dialyzer specs shotcuts;
* `veon_pdu` application is responsible for parsing, validating and rendering application protocol data units;
* `veon_storage` application is a layer of internal data storages abstraction;
* `veon` application is responsible for internal business-logic and works with `veon_storage` closely;
* `veon_web` is responsible for web-interface of the whole application.

Also, `opt/` directory contains some simple test written in python.

Main idea of this application is to solve race conditions with consistent sharding of incoming requests. Every request requires `imdb_id` key to calculate which `veon_worker` is responsible for handling it. In fact, all requests for the same `imdb_id` are serialized in this way, so no race condition is possible. This kind solution was tested in production in some different environment (`riak_core` was used to build a ring of workers).

To keep the installation light and easy to deploy I decided to use internal ETS tables as data storage.

## PDUs

### Movie register request
```json
{
    "imdbId": "UDK6P8VH48QXHBDA",
    "screenId": "9KY00WGHMRWQ3FDL",
    "availableSeats": 3
}
```

### Movie register response
```json
{
    "meta": {
        "status": true,
        "code": 201,
        "slogan": "Created"
    },
    "imdbId": "UDK6P8VH48QXHBDA",
    "movieTitle": "Stub-Movie-Title",
    "availableSeats": 3,
    "reservedSeats": 0,
    "screenId": "9KY00WGHMRWQ3FDL"
}
```

### Movie retrieve request
```json
{
    "imdbId": "57RHI723LV1BB6L2",
    "screenId": "Q18KFSAM3A1N6S8K"
}
```

### Movie retrieve response
```json
{
    "meta": {
        "status": true,
        "code": 200,
        "slogan": "OK"
    },
    "imdbId": "57RHI723LV1BB6L2",
    "movieTitle": "Stub-Movie-Title",
    "availableSeats": 8,
    "reservedSeats": 0,
    "screenId": "Q18KFSAM3A1N6S8K"
}
```

### Seat reserve request
```json
{
    "imdbId": "FQ7F258QEYUYB76B",
    "screenId": "NQ92Y0C9OE53NQ3Q"
}
```

### Seat reserve response
```json
{
    "meta": {
        "status": true,
        "code": 201,
        "slogan": "Created"
    },
    "imdbId": "FQ7F258QEYUYB76B",
    "movieTitle": "Stub-Movie-Title",
    "reservationId": "93747954-39d2-43bb-8dea-348b2fedef52",
    "screenId":"NQ92Y0C9OE53NQ3Q"
}
```

### Seat reserve cancellation request
```json
{
    "imdbId": "3VF50YR0P5JMEZ35",
    "screenId": "D0P83J6UDAK2A9IH",
    "reservationId": "cc381c7d-64b8-4522-aaa8-8cd5ccb92ce7"
}
```

### Seat reserve cancellation response
```json
{
    "meta": {
        "status": true,
        "code": 200,
        "slogan": "OK"
    },
    "imdbId": "3VF50YR0P5JMEZ35",
    "movieTitle": "Stub-Movie-Title",
    "availableSeats": 1,
    "reservedSeats": 0,
    "screenId": "D0P83J6UDAK2A9IH"
}
```

### Generic error response
```json
{
    "meta": {
        "status": false,
        "code": 404,
        "slogan": "Not Found"
    }
}
```

## Known issues

Despite the fact that the application has been written so that it will be developed in the future, in the development process I managed to notice a few things that I do not like.

### Go REST-like

`veon_web` application is using basic POST requests and does not utilize REST-like approach to handle CRUD applications.

### Some inconsistency in interface PDUs

You should notice some inconsistency in PDUs. For example, when reserving the seat you will get `availableSeats` and `reservedSeats` parameters in response. When cancelling the reservation, you will not. This inconsistency should be fixed.

### Confusing naming

Some things are named differently in different parts of the application, which causes some confusion. For example, `/movie/lookup` URI, but `veon_pdu_movie_retrieve_*` PDUs.

### Lack of tests

Lots of even positive scenarios are not tested. I just didn't have enough time for it. For example, testcase for `reserved_seats` counter decrementation.

### Lack of logging

When facing production issues the most helpful things you can get are logs. For now application contains like a couple of `lager` calls.

### No configuration

I decided to hardcode that few things that should be configurable. Of course, this is one of the things that should be fixed.

### Weak internal data structures

`veon_storage` application seems not to be ready for data to be written to external DB as it stores reserved seats too close to the movie itself. Probably, internal data structures should be more "relational".

### Application is waiting for some refactoring

Despite the fact this application is barely written, there are some lines of code that should be refactored. For example, you can look at cowboy handlers and lots of boilerplate code.

## Some code snippets

All of this snippets are written to be copypasted to the erlang REPL.

### Register movie request flow.
```erlang
Json = <<"{\"imdbId\": \"tt0111161\",\"availableSeats\": 100,\"screenId\": \"screen_123456\"}">>.
Doc = veon_helper_jiffy:unpack(veon_pdu:parse_json(Json)).
{ok, ValidDoc} = veon_pdu_movie_register_req:validate(Doc).
veon_pdu_movie_register_req:from_document(ValidDoc).
```

### Register movie response flow.
```erlang
Success = true.
Code = veon_pdu:code('ok').
Slogan = veon_pdu:slogan(Code).
ImdbId = <<"tt0111161">>.
AvailableSeats = 100.
ReservedSeats = 0.
ScreenId = <<"screen_123456">>.
Rec = veon_pdu_movie_register_res:new(Success, Code, Slogan, ImdbId, <<"STUB">>, AvailableSeats, ReservedSeats, ScreenId).
{ok, Doc} = veon_pdu_movie_register_res:to_document(Rec).
{ok, Json} = veon_pdu_movie_register_res:validate(Doc).
veon_pdu:render_json(Json).
```

### Reserve seat request flow.
```erlang
Json = <<"{\"imdbId\": \"tt0111161\", \"screenId\": \"screen_123456\"}">>.
Doc = veon_helper_jiffy:unpack(veon_pdu:parse_json(Json)).
{ok, ValidDoc} = veon_pdu_movie_reserve_req:validate(Doc).
veon_pdu_movie_reserve_req:from_document(ValidDoc).
```

### Reserve seat response flow.
```erlang
Success = true.
Code = veon_pdu:code('ok').
Slogan = veon_pdu:slogan(Code).
ImdbId = <<"tt0111161">>.
ReservationId = <<"B0B61833-6867-438B-BE3F-30522086B5A6">>.
ScreenId = <<"screen_123456">>.
Rec = veon_pdu_movie_reserve_res:new(Success, Code, Slogan, ImdbId, <<"STUB">>, ReservationId, ScreenId).
{ok, Doc} = veon_pdu_movie_reserve_res:to_document(Rec).
{ok, Json} = veon_pdu_movie_reserve_res:validate(Doc).
veon_pdu:render_json(Json).
```

### Retrieve movie request flow.
```erlang
Json = <<"{\"imdbId\": \"tt0111161\", \"screenId\": \"screen_123456\"}">>.
Doc = veon_helper_jiffy:unpack(veon_pdu:parse_json(Json)).
{ok, ValidDoc} = veon_pdu_movie_retrieve_req:validate(Doc).
veon_pdu_movie_retrieve_req:from_document(ValidDoc).
```

### Retrieve movie response flow.
```erlang
Success = true.
Code = veon_pdu:code('ok').
Slogan = veon_pdu:slogan(Code).
ImdbId = <<"tt0111161">>.
AvailableSeats = 100.
ReservedSeats = 16.
ScreenId = <<"screen_123456">>.
Rec = veon_pdu_movie_retrieve_res:new(Success, Code, Slogan, ImdbId, <<"STUB">>, AvailableSeats, ReservedSeats, ScreenId).
{ok, Doc} = veon_pdu_movie_retrieve_res:to_document(Rec).
{ok, Json} = veon_pdu_movie_retrieve_res:validate(Doc).
veon_pdu:render_json(Json).
```

### Generic error response flow.
```erlang
Success = false.
Code = veon_pdu:code('not-found').
Slogan = veon_pdu:slogan(Code).
Rec = veon_pdu_movie_generic_error:new(Success, Code, Slogan).
{ok, Doc} = veon_pdu_movie_generic_error:to_document(Rec).
{ok, Json} = veon_pdu_movie_generic_error:validate(Doc).
veon_pdu:render_json(Json).
```

### Internal storage operations flow.
```erlang
M0 = veon_storage:new_movie(<<"imdb_id">>, <<"title">>, 5, <<"screen_id">>).
R0 = veon_storage:new_reserve().
{ok, M1} = veon_storage:add_reserve(M0, R0).
{ok, M2} = veon_storage:add_reserve(M1, veon_storage:new_reserve()).
{ok, M3} = veon_storage:add_reserve(M2, veon_storage:new_reserve()).
veon_storage:reservation_count(M3).
{ok, M4} = veon_storage:add_reserve(M3, veon_storage:new_reserve()).
{ok, M5} = veon_storage:add_reserve(M4, veon_storage:new_reserve()).
veon_storage:reservation_count(M5).
{error, reservations_exceeded} = veon_storage:add_reserve(M5, veon_storage:new_reserve()).
{error, not_found} = veon_storage:remove_reserve(M5, <<"fake_reserve">>).
{ok, M6} = veon_storage:remove_reserve(M5, veon_storage:reservation_id(R0)).
{ok, M6} = veon_storage:remove_reserve(M5, R0).
veon_storage:reservation_count(M6).
```

### ETS storage operation flow.
```erlang
{ok, S} = veon_storage_ets:new().
M0 = veon_storage:new_movie(<<"imdb_id">>, <<"title">>, 5, <<"screen_id">>).
veon_storage_ets:store(S, M0).
{ok, M0} = veon_storage_ets:lookup(S, veon_storage:imdb_id(M0), veon_storage:screen_id(M0)).
R0 = veon_storage:new_reserve().
{ok, M1} = veon_storage:add_reserve(M0, R0).
veon_storage_ets:store(S, M1).
{ok, M1} = veon_storage_ets:lookup(S, veon_storage:imdb_id(M0), veon_storage:screen_id(M0)).
```

### Internal workers flow.
```erlang
ImdbId = <<"tt0111161">>.
MovieTitle = <<"STUB">>.
AvailableSeats = 5.
ScreenId = <<"screen_123456">>.
WorkerId = veon_shard:worker_by_id(veon_shard:id_for(ImdbId)).
{ok, M0} = veon_worker:store_movie(WorkerId, ImdbId, MovieTitle, AvailableSeats, ScreenId).
{ok, {M1, R0}} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{ok, M2} = veon_worker:remove_reserve(WorkerId, ImdbId, ScreenId, veon_storage:reservation_id(R0)).
{error, not_found} = veon_worker:remove_reserve(WorkerId, ImdbId, ScreenId, veon_storage:reservation_id(R0)).
{ok, {M3, _}} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{ok, {M4, _}} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{ok, {M5, _}} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{ok, {M6, _}} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{ok, {M7, _}} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{error, reservations_exceeded} = veon_worker:add_reserve(WorkerId, ImdbId, ScreenId).
{ok, M7} = veon_worker:lookup_movie(WorkerId, ImdbId, ScreenId).
{error, not_found} = veon_worker:lookup_movie(WorkerId, ImdbId, <<"fake_screen">>).
```
