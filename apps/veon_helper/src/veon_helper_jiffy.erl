-module(veon_helper_jiffy).

-export([get/2, unpack/1]).


%% Interface


-spec get(Path :: jiffy_v:jv_ret_stack(), Document :: jiffy_v:jv_data() | undefined) ->
    veon_helper_type:generic_return(OkRet :: any(), ErrorRet :: undefined).

get(_Path, undefined) ->
    {error, undefined};

get(Key, Value) when not is_list(Key) ->
    get([Key], Value);

get([], Value) ->
    {ok, Value};

get([Key | Path], {Proplist}) ->
    Struct = proplists:get_value(Key, Proplist),
    get(Path, Struct);

get(Path, _Value) ->
    get(Path, undefined).


-spec unpack(
    In :: veon_helper_type:generic_return(OkRet :: any(), ErrorRet :: undefined)
) ->
    Ret :: any() | undefined.

unpack({ok, Value}) ->
    Value;

unpack({error, undefined}) ->
    undefined.
