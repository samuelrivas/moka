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

%%% @doc Test to verify that moka_server works correctly with the rest of the
%%% modules
-module(moka_server_tests).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================

%% Test we can start two concurrent mokas and use them normally
concurrent_moka_test_() ->
    {setup,
     fun() ->
             Apps  = sel_application:start_app(moka),
             Moka1 = moka:start(moka_server_tests_aux1),
             Moka2 = moka:start(moka_server_tests_aux2),
             {Apps, Moka1, Moka2}
     end,
     fun({Apps, Moka1, Moka2}) ->
             moka:stop(Moka2),
             moka:stop(Moka1),
             sel_application:stop_apps(Apps)
     end,
     fun({_Apps, Moka1, Moka2}) ->
             {inorder,
              [?_test(moka:replace(Moka1, something, fun() -> bar end)),
               ?_test(moka:replace(Moka1, external, something, fun() -> hoge end)),
               ?_test(moka:replace(Moka2, something, fun() -> baz end)),
               ?_test(moka:replace(Moka2, external, something, fun() -> piyo end)),
               ?_test(moka:load(Moka1)),
               ?_test(moka:load(Moka2)),
               ?_assertEqual(bar, moka_server_tests_aux1:call_something()),
               ?_assertEqual(hoge, moka_server_tests_aux1:call_external()),
               ?_assertEqual(baz, moka_server_tests_aux2:call_something()),
               ?_assertEqual(piyo, moka_server_tests_aux2:call_external())]}
     end}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
