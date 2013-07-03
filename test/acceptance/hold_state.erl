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

%%% @doc acceptance tests for stateful moking functionality
%%%
%%% Just for fun, we use crypto as target, as it represents very nicely a module
%%% offering functionality with complex state involved
%%%
%%% Note we mok an auxiliary module (hold_state_aux) instead of calling crypto
%%% directly in this module as we have to be sure we reload code after we load
%%% the moka. Even if we exported functions to call ?MODULE: to use the new
%%% version of the code, the test will run always the old version of the code
%%% until it finishes. This would complicate things a bit, for example we would
%%% fail trying to unload the moka (or kill the test process if moka were not
%%% gentle enough to check for lingering process before unloading the code).

-module(hold_state).

%%%_* Exports ==========================================================

%%%_* Includes =========================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================
can_base_on_previous_results_test_() ->
    {setup,
     fun() ->
             Apps = sel_application:start_app(moka),
             Moka = moka:start(hold_state_aux),
             moka:replace(Moka, crypto, rand_uniform, count_entries_fun(Moka)),
             moka:load(Moka),
             {Apps, Moka}
     end,

     cleanup_fun(),

     %% Now we should be able to have a stateful, but controlled
     %% "pseudorandom" generator that just counts forward
     {inorder,
      [?_assertEqual(0, hold_state_aux:rand_uniform(0, 10)),
       ?_assertEqual(1, hold_state_aux:rand_uniform(0, 10)),
       ?_assertEqual(2, hold_state_aux:rand_uniform(0, 10)),
       ?_assertEqual(3, hold_state_aux:rand_uniform(0, 10))]}}.

can_check_arg_sequence_test_() ->
    {setup,
     fun() ->
             Apps = sel_application:start_app(moka),
             Moka = moka:start(hold_state_aux),
             moka:replace(
               Moka, crypto, rand_uniform, sum_previous_args_fun(Moka)),
             moka:load(Moka),
             {Apps, Moka}
     end,

     cleanup_fun(),

     %% Now the "pseudorandom" generator should sum all previous first
     %% arguments.
     {inorder,
      [?_assertEqual(0, hold_state_aux:rand_uniform(1, 10)),
       ?_assertEqual(1, hold_state_aux:rand_uniform(2, 10)),
       ?_assertEqual(3, hold_state_aux:rand_uniform(5, 10)),
       ?_assertEqual(8, hold_state_aux:rand_uniform(0, 10))]}}.

can_count_function_calls_test_() ->
    {setup,
     fun() ->
             Apps = sel_application:start_app(moka),
             Moka = moka:start(hold_state_aux),

             moka:replace(Moka, crypto, rand_uniform, fun(_, _) -> 0 end),
             moka:replace(Moka, crypto, rand_bytes, fun(_) -> <<>> end),
             moka:replace(Moka, internal_call, fun() -> foo end),
             moka:load(Moka),

             %% Do some calls
             hold_state_aux:rand_uniform(foo, bar),
             hold_state_aux:rand_bytes(foo),
             hold_state_aux:rand_bytes(foo),
             hold_state_aux:rand_uniform(foo, bar),
             hold_state_aux:rand_uniform(foo, bar),

             %% And also some internal calls
             hold_state_aux:call_to_internal(),

             {Apps, Moka}
     end,
     cleanup_fun(),

     %% Check we can count the calls
     fun({_, Moka}) ->
             [?_assertEqual(3, calls(Moka, crypto, rand_uniform)),
              ?_assertEqual(2, calls(Moka, crypto, rand_bytes)),
              ?_assertEqual(1, calls(Moka, hold_state_aux, internal_call))]
     end}.

%%%_* Internal Functions ===============================================

cleanup_fun() ->
    fun({Apps, Moka}) ->
            moka:stop(Moka),
            sel_application:stop_apps(Apps)
    end.

count_entries_fun(Moka) ->
    fun(_, _) ->
            case lists:reverse(moka:history(Moka)) of
                []                                        -> 0;
                [{_FunSpec, _Args, {return, Return}} | _] -> Return + 1
            end
    end.

sum_previous_args_fun(Moka) ->
    fun(_, _) ->
            History = moka:history(Moka),
            FoldFun =
                fun({_FunSpec, [Arg, _], _Return}, Acc) ->
                        Arg + Acc
                end,
            lists:foldl(FoldFun, 0, History)
    end.

calls(Moka, Module, Function) ->
    length(
      [x || {{M, F}, _Args, _Rtrn} <- moka:history(Moka),
            M =:= Module,
            F =:= Function]).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
