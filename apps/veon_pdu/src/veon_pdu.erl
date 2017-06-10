-module(veon_pdu).

-export([parse_json/1, render_json/1]).
-export([code/1, slogan/1]).


%% Types


-type meta_status() :: boolean().
-type meta_code() :: non_neg_integer().
-type meta_slogan() :: binary().

-export_type([meta_status/0, meta_code/0, meta_slogan/0]).


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


-spec code(Alias :: atom()) ->
    Code :: meta_code().

code('ok') -> 200;
code('created') -> 201;
code('bad-request') -> 400;
code('not-found') -> 404;
code('method-not-allowed') -> 405;
code('not-acceptable') -> 406;
code('conflict') -> 409;
code('internal-server-error') -> 500;
code('not-implemented') -> 501;
code('service-unavailable') -> 503;
code(_) -> error(invalid_error_alias).


-spec slogan(Code :: meta_code()) ->
    Slogan :: meta_slogan().

slogan(200) -> <<"OK">>;
slogan(201) -> <<"Created">>;
slogan(400) -> <<"Bad Request">>;
slogan(404) -> <<"Not Found">>;
slogan(405) -> <<"Method Not Allowed">>;
slogan(406) -> <<"Not Acceptable">>;
slogan(409) -> <<"Conflict">>;
slogan(500) -> <<"Internal Server Error">>;
slogan(501) -> <<"Not Implemented">>;
slogan(503) -> <<"Service Unavalable">>;
slogan(_) -> error(invalid_error_code).
