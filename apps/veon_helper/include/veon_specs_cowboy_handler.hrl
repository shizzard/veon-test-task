-spec init(
    Transport :: {atom(), http},
    Req :: cowboy_req:req(),
    Opts :: term()
) ->
    {ok, Req :: cowboy_req:req(), State :: state()} |
    {loop, Req :: cowboy_req:req(), State :: state()} |
    {loop, Req :: cowboy_req:req(), State :: state(), hibernate} |
    {loop, Req :: cowboy_req:req(), State :: state(), timeout()} |
    {loop, Req :: cowboy_req:req(), State :: state(), timeout(), hibernate} |
    {shutdown, Req :: cowboy_req:req(), State :: state()} |
    {upgrade, protocol, module()} |
    {upgrade, protocol, module(), Req :: cowboy_req:req(), Opts :: term()}.

-spec handle(
    Req :: cowboy_req:req(),
    State :: state()
) ->
    Ret :: {ok, Req :: cowboy_req:req(), State :: state()}.

-spec terminate(
    Reason :: {normal, shutdown} | {normal, timeout} | {error, closed} | {error, overflow} | {error, atom()},
    Req :: cowboy_req:req(),
    State :: state()
) ->
    ok.
