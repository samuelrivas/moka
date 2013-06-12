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

%%% @doc Acceptance tests for exception handling
-module(mok_exceptions).

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================

exceptions_test_() ->
    {setup,
     fun() ->
             Apps = sel_application:start_app(moka),
             Moka = moka:start(mok_exceptions_aux),
             moka:replace(Moka, internal, crashy_fun()),
             moka:load(Moka),
             {Apps, Moka}
     end,
     fun({Apps, Moka}) ->
             moka:stop(Moka),
             sel_application:stop_apps(Apps)
     end,
     [?_assertThrow(ouch,            mok_exceptions_aux:call({throw, ouch})),
      ?_assertError(ouch,            mok_exceptions_aux:call({error, ouch})),
      ?_assertExit (ouch,            mok_exceptions_aux:call({exit, ouch})),
      ?_assertError(function_clause, mok_exceptions_aux:call(foo))]}.

exceptions_history_test_() ->
    {setup,
     fun() ->
             Apps = sel_application:start_app(moka),
             Moka = moka:start(mok_exceptions_aux),
             moka:replace(Moka, internal, crashy_fun()),
             moka:load(Moka),
             %% Do some crashes
             catch mok_exceptions_aux:call({throw, ouch}),
             catch mok_exceptions_aux:call({error, doh}),
             catch mok_exceptions_aux:call({exit,  gna}),
             {Apps, Moka}
     end,
     fun({Apps, Moka}) ->
             moka:stop(Moka),
             sel_application:stop_apps(Apps)
     end,
     fun({_Apps, Moka}) ->
             History = moka:history(Moka),
             [match_exception_(throw, ouch, lists:nth(1, History)),
              match_exception_(error, doh , lists:nth(2, History)),
              match_exception_(exit , gna , lists:nth(3, History))]
     end}.

%%%_* Private Functions ================================================

match_exception_(Class, Reason, HistoryEntry) ->
    ?_assertMatch(
       {{mok_exceptions_aux, internal},
        _Args,
        {exception, Class, Reason}},
       HistoryEntry
      ).

crashy_fun() ->
    fun({throw, X}) -> throw(X);
       ({exit,  X}) -> exit(X);
       ({error, X}) -> error(X)
    end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
