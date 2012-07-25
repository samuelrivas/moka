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

-module(moka_fsm_proper).

-behaviour(proper_fsm).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          replaced = [] :: [{Fun::atom(), Arity::non_neg_integer()}]
         }).

%%% FSM Callbacks
-export([initial_state/0, initial_state_data/0, weight/3, precondition/4,
         postcondition/5, next_state_data/5]).

%%% properties
-export([prop_moka_fsm/0]).

%%% FSM States
-export([new/1, defined/1, loaded/1]).

%%% Transitions
-export([replace/3, call/2]).

%%%===================================================================
%%% Eunit Wrapper
%%%===================================================================
all_properties_test() ->
    ?assertEqual([], proper:module(?MODULE)).

%%%===================================================================
%%% FSM Callbacks
%%%===================================================================

initial_state() -> new.

initial_state_data() -> #state{}.

weight(_,_,_) -> 1.

%% TODO Specify the behaviour for repeated replaces in the same call spec
precondition(_From, _Target, State, {call, _, replace, Args}) ->
    not lists:member(get_fun_spec(Args), State#state.replaced);

precondition(_From, _Target, _State, _Call) -> true.


%% Try not to loose the postcondition too much or we might lose bugs
%%
%% Also, keep the last match-all clause falling through to false to avoid false
%% positives due to matching errors
postcondition(new, defined, _StateData, _Call, _Res) -> true;
postcondition(defined, defined, _StateData, _Call, _Res) -> true;
postcondition(defined, loaded, _StateData, _Call, _Res) ->
    is_moked(origin_module());
postcondition(_From, _Target, State, {call, _Mod, call, [M, FunSpec]}, Res) ->
    case lists:member(FunSpec, State#state.replaced) of
        true ->
            {F, Arity} = FunSpec,
            Res == {moked, M, F, Arity};
        false ->
            expected_exception(Res)
    end;

postcondition(_From, _Target, _StateData, _Call, _Res) -> false.

next_state_data(_From, _Target, State, _, {call, _, replace, Args}) ->
    State#state{replaced = [get_fun_spec(Args) | State#state.replaced]};
next_state_data(_From, _Target, State, _Res, _Call) ->
    State.

%%%===================================================================
%%% States
%%%===================================================================
new(_) ->
    [{defined, call_replace()}
     , {new, call_function()}].

defined(_) ->
    [{defined, call_replace()}
     , {defined, call_function()}
     , {loaded, {call, moka, load, [{var, moka}]}}].

loaded(_) ->
    [{loaded, call_function()}].

call_replace() ->
    {call, ?MODULE, replace, [{var, moka}, dest_module(), dest_funct_spec()]}.

call_function() ->
    {call, ?MODULE, call, [origin_module(), orig_fun_spec()]}.

%%%===================================================================
%%% Generators
%%%===================================================================

dest_funct_spec() ->
    RealFuncts = get_exported(dest_module()),

    %% For now, you must modify this list manually if you modify the origin
    %% module
    FakeFuncts = [{unimplemented, 0}],

    proper_types:elements(RealFuncts ++ FakeFuncts).

orig_fun_spec() ->
    proper_types:elements(get_exported(origin_module())).

get_exported(Mod) ->
    [Funct || Funct = {Name, _Arity} <- Mod:module_info(exports)
                  , Name /= module_info].

%%%===================================================================
%%% Transitions
%%%===================================================================
replace(Moka, Mod, {FunctName, Arity}) ->
    moka:replace(Moka, Mod, FunctName, make_fun(Mod, FunctName, Arity)).

call(Module, {Funct, Arity}) ->
    try erlang:apply(Module, Funct, make_args(Arity))
    of Result -> {ok, Result}
    catch Type:Reason -> {exception, Type, Reason, erlang:get_stacktrace()}
    end.

%%%===================================================================
%%% Properties
%%%===================================================================
prop_moka_fsm() ->
    ?FORALL(
       Cmds, proper_fsm:commands(?MODULE),
       ?TRAPEXIT(
          begin
              Moka = moka:start(origin_module()),
              try
                  {H, S, R} =
                      proper_fsm:run_commands(?MODULE, Cmds, [{moka, Moka}]),

                  ?WHENFAIL(
                     report_error(Cmds, H, S, R),

                     proper:conjunction(
                       [{result_is_ok, R =:= ok}
                        , {code_restored, not is_moked(origin_module())}
                       ]))
              after
                  moka:stop(Moka)
              end
          end)).

%%%===================================================================
%%% Auxiliary functions
%%%===================================================================

origin_module() -> moka_test_origin_module.

dest_module() -> moka_test_dest_module.

make_fun(M, F, Arity) -> make_fun(Arity, {moked, M, F, Arity}).

%% We support only arities up to 3, you need to update this if you modify the
%% destination module
make_fun(N, Return) when N =:= 0 -> fun() -> Return end;
make_fun(N, Return) when N =:= 1 -> fun(_) -> Return end;
make_fun(N, Return) when N =:= 2 -> fun(_, _) -> Return end;
make_fun(N, Return) when N =:= 3 -> fun(_, _, _) -> Return end.

get_fun_spec([_Moka, _Dest, FunSpec]) -> FunSpec;
get_fun_spec([_Dest, FunSpec]) -> FunSpec.

make_args(N) -> lists:duplicate(N, fake_arg).

expected_exception({exception, error, undef, _}) -> true;
expected_exception({exception, error, {called, _Mod, _Line}, _}) -> true;
expected_exception(_) -> false.

%% TODO Move this to a generic library
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

is_moked(Module) ->
    lists:keymember(moka_orig_module, 1, Module:module_info(attributes)).
