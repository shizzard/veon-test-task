-module(veon_web).

-export([
    do_generic_checks/1,
    ok/2, created/2,
    bad_request/1, not_found/1, method_not_allowed/1, conflict/1,
    internal_server_error/1, not_implemented/1
]).


%% Interface


-spec ok(
    Document :: jiffy_v:jv_data(),
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

ok(Document, Req) ->
    reply(Document, Req, 200).


-spec created(
    Document :: jiffy_v:jv_data(),
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

created(Document, Req) ->
    reply(Document, Req, 201).


-spec bad_request(
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

bad_request(Req) ->
    generic_error(Req, 'bad-request').


-spec not_found(
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

not_found(Req) ->
    generic_error(Req, 'not-found').


-spec method_not_allowed(
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

method_not_allowed(Req) ->
    generic_error(Req, 'method-not-allowed').


-spec conflict(
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

conflict(Req) ->
    generic_error(Req, 'conflict').


-spec internal_server_error(
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

internal_server_error(Req) ->
    generic_error(Req, 'internal-server-error').


-spec not_implemented(
    Req :: cowboy_req:req()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

not_implemented(Req) ->
    generic_error(Req, 'not-implemented').


-spec generic_error(
    Req :: cowboy_req:req(),
    Alias :: atom()
) ->
    veon_helper_type:ok_return(
        OkRet :: cowboy_req:req()
    ).

generic_error(Req, Alias) ->
    Code = veon_pdu:code(Alias),
    Slogan = veon_pdu:slogan(Code),
    Rec = veon_pdu_movie_generic_error:new(false, Code, Slogan),
    {ok, Document} = veon_pdu_movie_generic_error:to_document(Rec),
    {ok, Body} = veon_pdu_movie_generic_error:validate(Document),
    reply(Body, Req, Code).


-spec reply(
    Document :: jiffy_v:jv_data(),
    Req :: cowboy_req:req(),
    Status :: cowboy:http_status()
) ->
    veon_helper_type:generic_return(
        OkRet :: cowboy_req:req()
    ).

reply(Document, Req, StatusCode) ->
    {ok, Body} = veon_pdu:render_json(Document),
    cowboy_req:reply(StatusCode, [], Body, Req).


-spec do_generic_checks(Req :: cowboy_req:req()) ->
    veon_helper_type:generic_return(
        OkRet :: {Req :: cowboy_req:req(), Document :: jiffy_v:jv_data()},
        ErrorRet :: cowboy_req:req()
    ).

do_generic_checks(Req) ->
    {Method, Req2} = cowboy_req:method(Req),
    do_generic_checks_1_method(Method, Req2).


-spec do_generic_checks_1_method(Method :: binary(), Req :: cowboy_req:req()) ->
    veon_helper_type:generic_return(
        OkRet :: {Req :: cowboy_req:req(), Document :: jiffy_v:jv_data()},
        ErrorRet :: cowboy_req:req()
    ).

do_generic_checks_1_method(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    do_generic_checks_2_body(HasBody, Req);

do_generic_checks_1_method(_, Req) ->
    {error, method_not_allowed(Req)}.


-spec do_generic_checks_2_body(HasBody :: boolean(), Req :: cowboy_req:req()) ->
    veon_helper_type:generic_return(
        OkRet :: {Req :: cowboy_req:req(), Document :: jiffy_v:jv_data()},
        ErrorRet :: cowboy_req:req()
    ).

do_generic_checks_2_body(true, Req) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    do_generic_checks_3_json(Body, Req1);

do_generic_checks_2_body(false, Req) ->
    {error, bad_request(Req)}.


-spec do_generic_checks_3_json(Body :: binary(), Req :: cowboy_req:req()) ->
    veon_helper_type:generic_return(
        OkRet :: {Req :: cowboy_req:req(), Document :: jiffy_v:jv_data()},
        ErrorRet :: cowboy_req:req()
    ).

do_generic_checks_3_json(Body, Req) ->
    case veon_pdu:parse_json(Body) of
        {ok, Document} ->
            {ok, {Req, Document}};
        {error, Error} ->
            lager:error("Error parsing JSON: ~p", [Error]),
            {error, bad_request(Req)}
    end.
