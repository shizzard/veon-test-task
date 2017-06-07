-spec init(
    Args :: term()
) ->
    {ok, State :: state()} |
    {ok, State :: state(), non_neg_integer() | hibernate} |
    {stop, Reason :: term()} |
    ignore.

-spec handle_call(
    Request :: veon_helper_type:gen_server_generic_message(),
    From :: veon_helper_type:gen_server_generic_from(),
    State :: state()
) ->
    veon_helper_type:gen_server_return(state()).

-spec handle_cast(
    Request :: veon_helper_type:gen_server_generic_message(),
    State :: state()
) ->
    veon_helper_type:gen_server_noreply(state()) |
    veon_helper_type:gen_server_stop_noreply(state()).

-spec handle_info(
    Request :: veon_helper_type:gen_server_generic_message(),
    State :: state()
) ->
    veon_helper_type:gen_server_noreply(state()) |
    veon_helper_type:gen_server_stop_noreply(state()).

-spec terminate(
    Reason :: term(),
    State :: state()
) ->
    veon_helper_type:ok_return().

-spec code_change(
    OldVsn :: term(),
    State :: state(),
    Extra :: term()
) ->
    veon_helper_type:ok_return(Ret :: state()).
