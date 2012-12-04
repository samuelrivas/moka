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

%%% @doc Tests to verify the examples written in the README work

%%% NOTE These tests are not meant to be good tests (actually they are pretty
%%% bad tests) but just to raise an alarm if there is an inconsistency between
%%% what is written in the documentation (the README) and what the
%%% implementation actually does. If you need to fix something here, it means
%%% you should also update the README

-module(readme_code).

%%%_* Includes =========================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Test Cases =======================================================

demo_test_() ->
    {setup,
     fun() -> sel_application:start_app(moka) end,
     fun(Apps) -> sel_application:stop_apps(Apps) end,
     fun(Apps) ->
             ExpectedMoka = short_demo_moka,
             {inorder,
              [?_assertEqual([samerlib, moka], Apps),
               ?_assertEqual(
                  {error, enoent}, short_demo:read_a_file("Whatever.txt")),
               ?_assertEqual(ExpectedMoka, moka:start(short_demo)),
               ?_assertEqual(
                  ok,
                  moka:replace(
                    ExpectedMoka, file, read_file,
                    fun(File) ->
                            "So you really want to read " ++ File ++ ", do you?"
                    end)),
               ?_assertEqual(ok, moka:load(ExpectedMoka)),
               ?_assertEqual(
                  "So you really want to read Whatever.txt, do you?",
                  short_demo:read_a_file("Whatever.txt")),
               ?_assertEqual({error, enoent}, file:read_file("Whatever.txt")),
               ?_assertEqual(ok, moka:stop(ExpectedMoka)),
               ?_assertEqual(
                  {error, enoent},  short_demo:read_a_file("Whatever.txt"))]}
     end}.

%%%_* Private Functions ================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
