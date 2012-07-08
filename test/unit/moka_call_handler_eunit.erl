%%% @doc Unit tests for {@link moka_call_handler}

-module(moka_call_handler_eunit).

-include_lib("eunit/include/eunit.hrl").

call_handler_test_() ->
    {setup,
     fun() ->
             Handler = moka_call_handler:start_link(),
             moka_call_handler:set_response_fun(Handler, fun(X,Y) -> X * Y end),
             Handler
     end,
     fun(Handler) ->
             moka_call_handler:stop(Handler),
             %% Assert the handler actually stops. This will timeout otherwise
             sel_process:wait_exit(Handler)
     end,
     fun(Handler) ->
             [?_assertEqual(
                 0, moka_call_handler:get_response(Handler, [0, 1]))

              , ?_assertEqual(
                   6, moka_call_handler:get_response(Handler, [2, 3]))
             ]
     end}.

