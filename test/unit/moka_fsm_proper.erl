%%% @doc fsm test for moka

-module(moka_fsm_proper).

-behaviour(proper_fsm).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").

-record(state, {
          replaced :: [{Fun::atom(), Arity::non_neg_integer()}]
         }).

%%% FSM Callbacks
-export([initial_state/0, initial_state_data/0, weight/3, precondition/4,
         postcondition/5, next_state_data/5]).

%%% properties
-export([prop_moka_fsm/0]).

%%% FSM States
-export([new/1, defined/1]).

%%% Transitions
-export([replace/3]).

%%%===================================================================
%%% FSM Callbacks
%%%===================================================================

initial_state() -> new.

initial_state_data() -> #state{}.

weight(_,_,_) -> 1.

precondition(_From, _Target, _State, _Call) -> true.


%% Try not to loose the postcondition too much or we might lose bugs
%%
%% Also, keep the last match-all clause falling through to false to avoid false
%% positives due to matching errors
postcondition(new, defined, _StateData, _Call, _Res) -> true;
postcondition(defined, defined, _StateData, _Call, _Res) -> true;

postcondition(_From, _Target, _StateData, _Call, _Res) -> false.

next_state_data(_From, _Target, State, _Call, _Res) -> State.

%%%===================================================================
%%% States
%%%===================================================================
new(_) ->
    [{defined,call_replace()}].

defined(_) ->
    [{defined, call_replace()}].

call_replace() ->
    {call, ?MODULE, replace, [{var, moka}, dest_module(), funct_spec()]}.

%%%===================================================================
%%% Generators
%%%===================================================================

funct_spec() ->
    Mod = dest_module(),
    RealFuncts =
        [Funct || Funct = {Name, _Arity} <- Mod:module_info(exports)
                      , Name /= module_info],

    %% For now, you must modify this list manually if you modify the origin
    %% module
    FakeFuncts = [{unimplemented, 0}],

    proper_types:elements(RealFuncts ++ FakeFuncts).

%%%===================================================================
%%% Transitions
%%%===================================================================
replace(Moka, Mod, {FunctName, Arity}) ->
    moka:replace(Moka, Mod, FunctName, make_fun(Arity)).

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
