-module(movie_add_reserve_handler).
-behavior(cowboy_http_handler).

-include_lib("veon_helper/include/veon_specs_cowboy_handler.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).
-type state() :: #state{}.


%% Interface


init(_Transport, Req, []) ->
    {ok, Req, #state{}}.


handle(Req, State) ->
    case veon_web:do_generic_checks(Req) of
        {ok, {Req1, Document}} ->
            handle_1_validate(Document, Req1, State);
        {error, Req1} ->
            {ok, Req1, State}
    end.


terminate(_Reason, _Req, _State) ->
    ok.


-spec handle_1_validate(
    Document :: jiffy_v:jv_data(),
    Req :: cowboy_req:req(),
    State :: state()
) ->
    Ret :: {ok, Req :: cowboy_req:req(), State :: state()}.

handle_1_validate(Document, Req, State) ->
    case veon_pdu_movie_reserve_req:validate(Document) of
        {ok, Document1} ->
            {ok, Record} = veon_pdu_movie_reserve_req:from_document(Document1),
            handle_2_reserve(Record, Req, State);
        {error, Errors} ->
            lager:error("Error validating movie reserve request: ~p", [Errors]),
            {ok, Req1} = veon_web:bad_request(Req),
            {ok, Req1, State}
    end.


-spec handle_2_reserve(
    Record :: veon_pdu_movie_reserve_req:pdu(),
    Req :: cowboy_req:req(),
    State :: state()
) ->
    Ret :: {ok, Req :: cowboy_req:req(), State :: state()}.

handle_2_reserve(Record, Req, State) ->
    Worker = veon_shard:worker_by_id(veon_shard:id_for(veon_pdu_movie_reserve_req:imdb_id(Record))),
    case veon_worker:add_reserve(
        Worker,
        veon_pdu_movie_reserve_req:imdb_id(Record),
        veon_pdu_movie_reserve_req:screen_id(Record)
    ) of
        {ok, {Movie, Reserve}} ->
            handle_3_generate_response({Movie, Reserve}, Req, State);
        {error, not_found} ->
            {ok, Req1} = veon_web:not_found(Req),
            {ok, Req1, State};
        {error, reservations_exceeded} ->
            %% Responding with conflict just to indicate situation
            {ok, Req1} = veon_web:conflict(Req),
            {ok, Req1, State}
    end.


-spec handle_3_generate_response(
    {
        Movie :: veon_storage:veon_storage_movie(),
        Reserve :: veon_storage:veon_storage_reserve()
    },
    Req :: cowboy_req:req(),
    State :: state()
) ->
    Ret :: {ok, Req :: cowboy_req:req(), State :: state()}.

handle_3_generate_response({Movie, Reserve}, Req, State) ->
    Code = veon_pdu:code('created'),
    Slogan = veon_pdu:slogan(Code),
    Response = veon_pdu_movie_reserve_res:new(
        true, Code, Slogan,
        veon_storage:imdb_id(Movie),
        veon_storage:movie_title(Movie),
        veon_storage:reservation_id(Reserve),
        veon_storage:screen_id(Movie)
    ),
    handle_4_validate_response(Response, Req, State).


-spec handle_4_validate_response(
    Record :: veon_pdu_movie_reserve_res:pdu(),
    Req :: cowboy_req:req(),
    State :: state()
) ->
    Ret :: {ok, Req :: cowboy_req:req(), State :: state()}.

handle_4_validate_response(Record, Req, State) ->
    {ok, Document} = veon_pdu_movie_reserve_res:to_document(Record),
    case veon_pdu_movie_reserve_res:validate(Document) of
        {ok, Document1} ->
            handle_5_reply(Document1, Req, State);
        {error, Errors} ->
            lager:error("Error validating movie reserve response: ~p", [Errors]),
            {ok, Req1} = veon_web:internal_server_error(Req),
            {ok, Req1, State}
    end.


-spec handle_5_reply(
    Document :: jiffy_v:jv_data(),
    Req :: cowboy_req:req(),
    State :: state()
) ->
    Ret :: {ok, Req :: cowboy_req:req(), State :: state()}.

handle_5_reply(Document, Req, State) ->
    {ok, Req1} = veon_web:created(Document, Req),
    {ok, Req1, State}.
