%%% Copyright (c) 2012, Samuel Rivas <samuelrivas@gmail.com>
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * Neither the name the author nor the names of its contributors may
%%%       be used to endorse or promote products derived from this software
%%%       without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%% @doc Unit tests for {@link moka_call_handler}

-module(moka_call_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------
call_handler_test_() ->
    {setup,
     fun() ->
             Handler = start_handler(fun(X,Y) -> X * Y end),
             Handler
     end,
     fun(Handler) -> stop_handler(Handler) end,
     fun(_) ->
             [?_assertEqual(0, get_response([0, 1])),
              ?_assertEqual(6, get_response([2, 3]))]
     end}.

%% Start a third process that acts as message relay, then one test process will
%% wait for a message from the relay and the other will send the message to the
%% relay. This test will not succeed unless both calls can be processed in
%% parallel by the call handler
parallel_test_() ->
    {"Call handler code can run in parallel",
     {setup,
      fun() ->
              Relay   = start_relay(),
              Handler = start_handler(
                          fun(wait) -> wait_message(Relay);
                             (send) -> send_message(Relay)
                          end),
              {Relay, Handler}
      end,
      fun({Relay, Handler}) ->
              stop_relay(Relay),
              stop_handler(Handler)
      end,
      fun(_) ->
              {inparallel,
               [?_assert(get_response([wait])),
                ?_test  (get_response([send]))]}
      end}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
handler_name() -> test_handler.

description() -> {a_module, a_function}.

start_handler(Fun) ->
    moka_call_handler:start_link(handler_name(), description(), Fun, none).

stop_handler(Pid) ->
    moka_call_handler:stop(handler_name()),
    sel_process:wait_exit(Pid).

get_response(Args) -> moka_call_handler:get_response(handler_name(), Args).

start_relay() -> spawn_link(fun() -> relay(no_msg) end).

stop_relay(Relay) ->
    erlang:exit(Relay, normal),
    sel_process:wait_exit(Relay).

wait_message(Relay) ->
    Relay ! {request, self()},
    receive msg     -> true
    after timeout() -> false
    end.

send_message(Relay) -> Relay ! msg.

relay(no_msg) ->
    receive msg     -> relay(msg)
    after timeout() ->
            ?debugMsg("Relay didn't receive any 'msg'"),
            ok
    end;
relay(msg) ->
    receive {request, Pid} -> Pid ! msg
    after timeout() ->
            ?debugMsg("Relay didn't receive any request"),
            ok
    end.

timeout() -> 1000.
