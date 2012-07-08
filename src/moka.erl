%%% @doc The moka main interface
%%%
%%% This module implements the operations to create new mokas and use them to
%%% mock calls from one module to other.

-module(moka).

-behaviour(gen_server).

%%%===================================================================
%%% Exports
%%%===================================================================
%% API
-export([start/1, stop/1, replace/4, load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% Types
%%%===================================================================
-record(state, {
          module        :: module() | undefined,
          abs_code      :: moka_mod_handler:abstract_code() | undefined,
          call_handlers = [] :: [moka_call_handler:call_handler()]
         }).

-type moka() :: pid().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new moka process
%%
%% `Mod' will be know as <b>the moked module</b> in this documentation. The
%% created Moka will be used to modify calls from this module.
%%
%% <b>NOTE:</b> You should never create two Mokas for the same module
%%
%% @todo Avoid the possibility of creating two mokas for the same module
-spec start(module()) -> moka().
start(Mod) -> crashfy:untuple(gen_server:start_link(?MODULE, Mod, [])).

%% @doc Stops a moka process
-spec stop(moka()) -> ok.
stop(Moka) -> sel_gen_server:call(Moka, stop).

%% @doc Substitutes all calls from the moked module to `Mod:Fun/N'
%%
%% The arity of `NewBehaviour' determines the arity of the substituted function
%%
%% @todo Errors when there are no calls to the substituted function
-spec replace(moka(), module(), atom(), fun()) -> ok.
replace(Moka, Module, Function, NewBehaviour) ->
    sel_gen_server:call(Moka, {replace, Module, Function, NewBehaviour}).

%% @doc Makes all substitutions effective
%%
%% The arity of `NewBehaviour' determines the arity of the substituted function
%%
%% @todo Errors when there are no calls to the substituted function
-spec load(moka()) -> ok.
load(Moka) -> sel_gen_server:call(Moka, load).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Mod) ->
    try
        {ok, #state{
           module = Mod,
           abs_code = moka_mod_utils:get_abs_code(Mod)
          }}
    catch
        Excpt -> {stop, {Excpt, erlang:get_stacktrace()}}
    end.

%% @private
handle_call(Request, From, State) ->
    try
        safe_handle_call(Request, From, State)
    catch
        Excpt ->
            {reply, {error, Excpt}, State};
        error:Reason ->
            Error = {Reason, erlang:get_stacktrace()},
            {stop, Error, {error, Error}, State}
    end.

%% FIXME We need to pass the original args, we must fix moka_mod_utils for this
%% FIXME This function is ugly, refactor
safe_handle_call({replace, Module, Function, NewBehaviour}, _From, State) ->
    {arity, Arity} = erlang:fun_info(NewBehaviour, arity),
    HandlerName = call_handler_name(State),
    moka_call_handler:start_link(HandlerName),
    moka_call_handler:set_response_fun(HandlerName, NewBehaviour),
    NewCode =
        moka_mod_utils:replace_remote_calls(
          {Module, Function, Arity},
          {moka_call_handler, get_response, [HandlerName, '$args']},
          State#state.abs_code),
    {reply, ok,
     State#state{
       call_handlers = [HandlerName | State#state.call_handlers],
       abs_code = NewCode}};

safe_handle_call(load, _From, State) ->
    moka_mod_utils:load_abs_code(State#state.module, State#state.abs_code),
    {reply, ok, State};

%% FIXME the module should be restored in the terminate
safe_handle_call(stop, _From, State) ->
    moka_mod_utils:restore_module(State#state.module),
    {stop, normal, ok, State};

safe_handle_call(Request, _From, _State) ->
    throw({bad_call, Request}).

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
call_handler_name(#state{module = Module, call_handlers = Handlers}) ->
    list_to_atom(
      lists:flatten(io_lib:format("~p_~p", [Module, length(Handlers)]))).
