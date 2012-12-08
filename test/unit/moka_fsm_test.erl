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

%%% @doc fsm test for moka
%%%
%%% This fsm exercises moka using the public api ({@link moka}). The goal is to
%%% explore as many combinations of api calls as possible. As there are unit
%%% tests for any other relevant module, this test focuses on covering
%%% interaction and possible weird states

-module(moka_fsm_test).

-behaviour(proper_fsm).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Types =============================================================

-type method_key()  :: {module(), Arity::non_neg_integer()}.
-type method_spec() :: {method_key(), ExpectedResult::term()}.

-record(state, {
          moka = none    :: none | moka:moka(),
          functions = [] :: [method_spec()]
         }).

%%%_* Exports ============================================================

%%% FSM Callbacks
-export([initial_state/0, initial_state_data/0, weight/3, precondition/4,
         postcondition/5, next_state_data/5]).

%%% properties
-export([prop_moka_fsm/0]).

%%% FSM States
-export([initial/1, new/1, loaded/1]).

%%% Transitions
-export([call/1, replace/2]).

%%%_* FSM Callbacks ====================================================

initial_state() -> initial.

initial_state_data() -> #state{functions = initial_funct_table()}.

weight(_,_,_) -> 1.

precondition(_From, _Target, _State, _Call) -> true.

%% Try not to loose the postcondition too much or we might lose bugs
%%
%% Also, keep the last match-all clause falling through to false to avoid false
%% positives due to matching errors
%%
%% Call postconditions take precedence over transition postconditions

%% Call postconditions
postcondition(_, loaded, StateData, {call, _, call, [{Funct, Arity}]}, Res) ->
    Res =:= get_expected_result(StateData#state.functions, Funct, Arity);
postcondition(_, _, _StateData, {call, _, call, [{Funct, Arity}]}, Res) ->
    Res =:= get_expected_result(initial_funct_table(), Funct, Arity);
postcondition(_, _, _StateData, {call, _Module, replace, _Args}, Res) ->
    Res =:= ok;

%% Transition postconditions
postcondition(initial, new, _StateData, _Call, _Res) ->
    not is_moked_module(origin_module());
postcondition(new, loaded, _StateData, _Call, _Res) ->
    is_moked_module(origin_module());
postcondition(_, initial, _StateData, _Call, _Res) ->
    not is_moked_module(origin_module());
postcondition(_From, _Target, _StateData, _Call, _Res) -> false.

next_state_data(_From, _Target, State, Res, {call, _, start, _}) ->
    State#state{moka = Res};
next_state_data(_From, _Target, State, _Res, {call, _, stop, _}) ->
    State#state{
      moka      = none,
      functions = initial_funct_table()
     };
next_state_data(_From, _Target, State, _Res, {call, _, replace, [_, Spec]}) ->
    replace_results(State, Spec);
next_state_data(_From, _Target, State, _Res, _Call) ->
    State.

%%%_* States ===========================================================

initial(State) ->
    [
     {new, {call, moka, start, [origin_module()]}},
     {initial, test_method_call(State)}
    ].

new(State) ->
    [
     {initial, {call, moka, stop, [State#state.moka]}},
     {new, test_method_call(State)},
     {new, {call, ?MODULE, replace, [State#state.moka, mokable_method()]}},
     {loaded, {call, moka, load, [State#state.moka]}}
    ].

loaded(State) ->
    [
     {initial, {call, moka, stop, [State#state.moka]}},
     {loaded, test_method_call(State)}
    ].

test_method_call(State) ->
    {call, ?MODULE, call, [test_method(State)]}.

%%%_* Generators =======================================================

test_method(State) -> proper_types:elements(all_test_methods(State)).

mokable_method() -> proper_types:elements(all_replaceable_methods()).

%%%_* Transitions ======================================================

call({Function, Arity}) ->
    try apply(origin_module(), Function, make_args(Arity))
    catch X:Y -> {exception, {X, Y}}
    end.

replace(Moka, {Module, Function, Arity}) ->
    moka:replace(Moka, Module, Function, replacement_fun(Arity)).

%%%_* Properties =======================================================

moka_fsm_test_() ->
    {setup,
     fun() -> sel_application:start_app(moka) end,
     fun(Apps) -> sel_application:stop_apps(Apps) end,
     ?_assert(proper:quickcheck(prop_moka_fsm()))}.

prop_moka_fsm() ->
    ?FORALL(
       Cmds, proper_fsm:commands(?MODULE),
       ?TRAPEXIT(
          begin
              {H, S, R} = proper_fsm:run_commands(?MODULE, Cmds),
              moka_main_sup:stop_all(),

              ?WHENFAIL(
                 report_error(Cmds, H, S, R),

                 proper:conjunction(
                   [{result_is_ok, proper:equals(R, ok)},
                    {code_restored, not is_moked_module(origin_module())}
                   ]))
          end)).

%%%_* Auxiliary functions ==============================================

%% TODO Move this to a generic library
%% FIXME If the property crashes due to an exception we are not printing the
%% command that causes it
report_error(Cmds, H, S, R) ->
    report_history_and_states(Cmds, H, S),
    report_last_state(S),
    report_result(R),
    %% io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,R]).
    ok.

report_history_and_states(Cmds, History, _States) ->
    lists:foreach(
      fun({Cmd, H}) -> pretty_print_step(Cmd, H) end,
      sel_lists:cut_and_zip(Cmds, History)).

pretty_print_step(Command, {{StateName, StateData}, Result}) ->
    print_line(),
    pretty_print_state(StateName, StateData),
    pretty_print_command(Command, Result).

pretty_print_state(StateName, StateData) ->
    io:format(
      "State     : ~p~n"
      "State Data: ~p~n",
      [StateName, StateData]).

pretty_print_command({set, Var, {call, Mod, Fun, Args}}, Result) ->
    io:format("~p=~p:~p(", [Var,Mod, Fun]),
    io:format(
      "~s) -> ~500p~n",
      [string:join([format("~p", [Arg]) || Arg <- Args], ","), Result]).

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

print_line() -> io:format("~s~n", [lists:duplicate(70, $-)]).

report_last_state({StateName, StateData}) ->
    print_line(),
    pretty_print_state(StateName, StateData).

report_result(R) ->
    print_line(),
    io:format("~p~n", [R]).

origin_module() -> moka_fsm_test_orig_module.

dest_module() -> moka_fsm_test_dest_module.

is_moked_module(Module) ->
    lists:keymember(moka_orig_module, 1, Module:module_info(attributes)).

%% This table contains the functions that are subject to be called during this
%% test. Note the expected result will change depending on the modifications
%% done with Moka during the test, so the state will keep an updated version of
%% this list
initial_funct_table() ->
    [
     {{direct_undef_dependency, 0}, {exception, {error, undef}}},
     {{direct_undef_dependency, 1}, {exception, {error, undef}}},
     {{direct_undef_dependency, 2}, {exception, {error, undef}}}
    ].

%% Returns a {function, arity} pair list
all_test_methods(State) -> [X || {X, _} <- State#state.functions].

get_expected_result(Table, Call, Arity) ->
    {_, Result} = sel_lists:keysearch({Call, Arity}, Table),
    Result.

%% This table contains the functions that are subject to be replaced with Moka
%% and a list of the affected functions (i.e. functions subject to be called
%% during the tests which expected result will change as a result of the
%% replacement)
replaceable_method_table() ->
    [
     {{dest_module(), unimplemented, 0}, [{direct_undef_dependency, 0}]},
     {{dest_module(), unimplemented, 1}, [{direct_undef_dependency, 1}]},
     {{dest_module(), unimplemented, 2}, [{direct_undef_dependency, 2}]}
    ].

replaced_spec_arity({_, _, Arity}) -> Arity.

all_replaceable_methods() -> [X || {X, _} <- replaceable_method_table()].

affected_by_replace(ReplacedFunction) ->
    Table         = replaceable_method_table(),
    {_, Affected} = sel_lists:keysearch(ReplacedFunction, Table),
    Affected.

replace_results(State, ReplacedSpec) ->
    AffectedFunctions = affected_by_replace(ReplacedSpec),
    Arity             = replaced_spec_arity(ReplacedSpec),
    NewResult         = {moked, make_args(Arity)},
    replace_results(State, AffectedFunctions, NewResult).

replace_results(State, AffectedFunctions, NewResult) ->
    State#state{
      functions =
          lists:map(
            fun({Fun, Expected}) ->
                    case lists:member(Fun, AffectedFunctions) of
                        false -> {Fun, Expected};
                        true  -> {Fun, NewResult}
                    end
            end,
            State#state.functions)}.

make_args(0) -> [];
make_args(N) -> lists:seq(0, N - 1).

replacement_fun(0) -> fun()     -> {moked, []}     end;
replacement_fun(1) -> fun(A)    -> {moked, [A]}    end;
replacement_fun(2) -> fun(A, B) -> {moked, [A, B]} end.
