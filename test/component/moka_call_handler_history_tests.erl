%%% Copyright (c) 2013, Samuel Rivas <samuelrivas@gmail.com>
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

%%% @doc
-module(moka_call_handler_history_tests).

%%%_* Exports ==========================================================
-export([]).

%%%_* Includes =========================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Types ============================================================

%%%_* Tests ============================================================

can_record_history_test_() ->
    {"The calls done from the call handler are recorded in the test server",
     setup,
     fun() ->
             init_servers(),
             moka_call_handler:get_response(call_handler_name(), [1, 2]),
             moka_call_handler:get_response(call_handler_name(), [3, 4])
     end,
     fun(_) -> stop_servers() end,
     {inorder,
      [?_assertEqual(
         expected_history([[1, 2], [3, 4]]),
          moka_history:get_calls(history_server()))]}}.

%%%_* Private Functions ================================================

init_servers() ->
    crashfy:untuple(moka_history:start_link(history_server())),
    moka_call_handler:start_link(
      call_handler_name(), call_description(), handler_fun(), history_server()).

stop_servers() ->
    moka_call_handler:stop(call_handler_name()),
    moka_history:stop(history_server()).

history_server() -> test_history_server.

call_handler_name() -> test_call_handler.

call_description() -> {test_module, test_funct}.

handler_fun() -> fun(A, B) -> {A, B} end.

expected_history(ArgsList) -> [history_entry(A, B) || [A, B] <- ArgsList].

history_entry(A, B) ->
    HandlerFun = handler_fun(),
    {call_description(), [A, B], HandlerFun(A, B)}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
