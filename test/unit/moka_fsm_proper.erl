%%% @doc fsm test for moka

-module(moka_fsm_proper).

-behaviour(proper_fsm).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

-record(state, {
          replaced = [] :: [{Fun::atom(), Arity::non_neg_integer()}]
         }).

%%% FSM Callbacks
-export([initial_state/0, initial_state_data/0, weight/3, precondition/4,
         postcondition/5, next_state_data/5]).

%%% properties
-export([prop_moka_fsm/0]).

%%% FSM States
-export([new/1, defined/1]).

%%% Transitions
-export([replace/3, call/2]).

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
postcondition(_From, _Target, State, {call, _Mod, call, [_M, FunSpec]}, Res) ->
    case lists:member(FunSpec, State#state.replaced) of
        true ->
            Res = properly_moked;
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
     , {defined, call_function()}].

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
    moka:replace(Moka, Mod, FunctName, make_fun(Arity)).

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

                  ?WHENFAIL(report_error(H, S, R), R =:= ok)
              after
                  moka:stop(Moka)
              end
          end)).

%%%===================================================================
%%% Auxiliary functions
%%%===================================================================

report_error(H, S, R) ->
    io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,R]).

origin_module() -> moka_test_origin_module.

dest_module() -> moka_test_dest_module.

%% We support only arities up to 3, you need to update this if you modify the
%% destination module
make_fun(N) when N =:= 0 -> fun() -> return_term(N) end;
make_fun(N) when N =:= 1 -> fun(_) -> return_term(N) end;
make_fun(N) when N =:= 2 -> fun(_, _) -> return_term(N) end;
make_fun(N) when N =:= 3 -> fun(_, _, _) -> return_term(N) end.

return_term(N) -> {moked_function_with_arity, N}.

get_fun_spec([_Moka, _Dest, FunSpec]) -> FunSpec;
get_fun_spec([_Dest, FunSpec]) -> FunSpec.

make_args(N) -> lists:duplicate(N, fake_arg).

expected_exception({exception, error, undef, _}) -> true;
expected_exception({exception, error, {called, _Mod, _Line}, _}) -> true;
expected_exception(_) -> false.
