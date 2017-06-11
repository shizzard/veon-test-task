Tested on Erlang 17.5.
WIP.

Register movie request flow.
```erlang
Json = <<"{\"imdbId\": \"tt0111161\",\"availableSeats\": 100,\"screenId\": \"screen_123456\"}">>.
Doc = veon_helper_jiffy:unpack(veon_pdu:parse_json(Json)).
{ok, ValidDoc} = veon_pdu_movie_register_req:validate(Doc).
veon_pdu_movie_register_req:from_document(ValidDoc).
```

Register movie response flow.
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

Reserve seat request flow.
```erlang
Json = <<"{\"imdbId\": \"tt0111161\", \"screenId\": \"screen_123456\"}">>.
Doc = veon_helper_jiffy:unpack(veon_pdu:parse_json(Json)).
{ok, ValidDoc} = veon_pdu_movie_reserve_req:validate(Doc).
veon_pdu_movie_reserve_req:from_document(ValidDoc).
```

Reserve seat response flow.
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

Retrieve movie request flow.
```erlang
Json = <<"{\"imdbId\": \"tt0111161\", \"screenId\": \"screen_123456\"}">>.
Doc = veon_helper_jiffy:unpack(veon_pdu:parse_json(Json)).
{ok, ValidDoc} = veon_pdu_movie_retrieve_req:validate(Doc).
veon_pdu_movie_retrieve_req:from_document(ValidDoc).
```

Retrieve movie response flow.
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

Generic error response flow.
```erlang
Success = false.
Code = veon_pdu:code('not-found').
Slogan = veon_pdu:slogan(Code).
Rec = veon_pdu_movie_generic_error:new(Success, Code, Slogan).
{ok, Doc} = veon_pdu_movie_generic_error:to_document(Rec).
{ok, Json} = veon_pdu_movie_generic_error:validate(Doc).
veon_pdu:render_json(Json).
```

Internal storage operations flow.
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

ETS storage operation flow.
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

Internal workers flow.
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
