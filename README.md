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
ImdbId = <<"tt0111161">>.
AvailableSeats = 100.
ReservedSeats = 0.
ScreenId = <<"screen_123456">>.
Rec = veon_pdu_movie_register_res:new(ImdbId, <<"STUB">>, AvailableSeats, ReservedSeats, ScreenId).
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
ImdbId = <<"tt0111161">>.
ReservationId = <<"B0B61833-6867-438B-BE3F-30522086B5A6">>.
ScreenId = <<"screen_123456">>.
Rec = veon_pdu_movie_reserve_res:new(ImdbId, <<"STUB">>, ReservationId, ScreenId).
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
ImdbId = <<"tt0111161">>.
AvailableSeats = 100.
ReservedSeats = 16.
ScreenId = <<"screen_123456">>.
Rec = veon_pdu_movie_retrieve_res:new(ImdbId, <<"STUB">>, AvailableSeats, ReservedSeats, ScreenId).
{ok, Doc} = veon_pdu_movie_retrieve_res:to_document(Rec).
{ok, Json} = veon_pdu_movie_retrieve_res:validate(Doc).
veon_pdu:render_json(Json).
```
