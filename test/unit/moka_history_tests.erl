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

%%% @doc unit tests for the moka history server
-module(moka_history_tests).

-export([]).

%%%_* Includes =========================================================
-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Eunit wrapper ====================================================
all_properties_test_() -> sel_test:props_to_eunit(?MODULE).

%%%_* Properties =======================================================
prop_get_history() ->
    ?FORALL(
       History, history(),
       try
           crashfy:untuple(moka_history:start_link(test_server_name())),
           replay_history(test_server_name(), History),
           proper:equals(History, moka_history:get_calls(test_server_name()))
       after
           moka_history:stop(test_server_name())
       end
      ).

%%%_* Generators =======================================================

history() -> proper_types:list(history_entry()).

history_entry() -> {function_gen(), args_gen(), return_gen()}.

%% Variety doesn't seem relevant for this test case, so we just generate a few
%% possibilities
function_gen() -> proper_types:elements([fun1, fun2, fun3, fun4]).
args_gen()     -> proper_types:list(proper_types:integer()).
return_gen()   -> proper_types:integer().

%%%_* Private Functions ================================================
test_server_name() -> moka_history_test_server.

replay_history(Server, History) ->
    lists:foreach(
      fun({Fun, Args, Result}) ->
              moka_history:add_call(Server, Fun, Args, Result)
      end,
      History).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
