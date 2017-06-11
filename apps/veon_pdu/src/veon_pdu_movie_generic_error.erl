-module(veon_pdu_movie_generic_error).
-behavior(veon_pdu).

-export([
    new/3, status/1, code/1, slogan/1,
    status/2, code/2, slogan/2,
    from_document/1, to_document/1, validate/1
]).

-record(?MODULE, {
    status :: veon_pdu:meta_status(),
    code :: veon_pdu:meta_code(),
    slogan :: veon_pdu:meta_slogan()
}).
-type pdu() :: #?MODULE{}.
-export_type([pdu/0]).

-include("datafield_definitions.hrl").


%% Interface


-spec new(
    Status :: veon_pdu:meta_status(),
    Code :: veon_pdu:meta_code(),
    Slogan :: veon_pdu:meta_slogan()
) ->
    pdu().

new(Status, Code, Slogan) ->
    #?MODULE{
        status = Status,
        code = Code,
        slogan = Slogan
    }.


-spec status(Record :: pdu()) ->
    Ret :: veon_pdu:meta_status().

status(#?MODULE{status = Status}) ->
    Status.


-spec status(Record :: pdu(), Value :: veon_pdu:meta_status()) ->
    Ret :: pdu().

status(#?MODULE{} = Record, Value) ->
    Record#?MODULE{status = Value}.


-spec code(Record :: pdu()) ->
    Ret :: veon_pdu:meta_code().

code(#?MODULE{code = Code}) ->
    Code.


-spec code(Record :: pdu(), Value :: veon_pdu:meta_code()) ->
    Ret :: pdu().

code(#?MODULE{} = Record, Value) ->
    Record#?MODULE{code = Value}.


-spec slogan(Record :: pdu()) ->
    Ret :: veon_pdu:meta_slogan().

slogan(#?MODULE{slogan = Slogan}) ->
    Slogan.


-spec slogan(Record :: pdu(), Value :: veon_pdu:meta_slogan()) ->
    Ret :: pdu().

slogan(#?MODULE{} = Record, Value) ->
    Record#?MODULE{slogan = Value}.


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
        veon_helper_jiffy:unpack(veon_helper_jiffy:get([?datafield_meta, ?datafield_status], Document)),
        veon_helper_jiffy:unpack(veon_helper_jiffy:get([?datafield_meta, ?datafield_code], Document)),
        veon_helper_jiffy:unpack(veon_helper_jiffy:get([?datafield_meta, ?datafield_slogan], Document))
    )}.


-spec to_document(Record :: pdu()) ->
    veon_helper_type:ok_return(
        OkRet :: jiffy_v:jv_data()
    ).

to_document(Record) ->
    {ok, {[
        {?datafield_meta, {[
            {?datafield_status, status(Record)},
            {?datafield_code, code(Record)},
            {?datafield_slogan, slogan(Record)}
        ]}}
    ]}}.


%% Internals


-spec validate_map() ->
    Ret :: jiffy_v:jv_type().

validate_map() ->
    {hash, [
        {?datafield_meta, required, {hash, [
            {?datafield_status, required, {boolean}},
            {?datafield_code, required, {integer}},
            {?datafield_slogan, required, {string}}
        ]}}
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

validate_fun_validate([?datafield_meta, ?datafield_slogan], Value)
when byte_size(Value) > 256 ->
    {error, <<"Slogan is too long">>};

validate_fun_validate(_Stack, _Value) ->
    {ok, valid}.


-spec validate_fun_fix(Stack :: jiffy_v:jv_ret_stack(), Value :: jiffy_v:jv_data()) ->
    Ret :: {ok, NewValue :: jiffy_v:jv_data()} | {error, invalid} | {error, Code :: jiffy_v:jv_ret_code()}.

validate_fun_fix(_Stack, _Value) ->
    {error, invalid}.
