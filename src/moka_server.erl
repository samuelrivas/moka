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

%%% @copyright 2012 Samuel Rivas
%%% @doc The main moka process
%%%
%%% Each moka server handles the moking for a single module. Moka servers must
%%% be started through the {@link moka_sup}
%%%
%%% You should not use the functions provided by this module directly, use the
%%% ones in {@link moka}
-module(moka_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([start_link/3, stop/1, replace/3, replace/4, export/3, load/1]).

%% This function should only be used for debugging moka
-deprecated([{stop, 1}]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Types ============================================================

-record(state, {
          module             :: module(),
          abs_code           :: moka_mod_utils:abstract_code(),
          handler_count = 0  :: non_neg_integer()
         }).

-type moka_server() :: atom().

-export_type([moka_server/0]).

%%%_* API ==============================================================

%% @doc Use {@link moka:start/2}
-spec start_link(moka_server(), module(), moka_mod_utils:abstract_code()) ->
                        {ok, pid()} | ignore | {error, term()}.
start_link(ServerName, Module, AbsCode) ->
    gen_server:start_link(
      {local, ServerName}, ?MODULE, {Module, AbsCode}, []).

%% @doc Stops a moka server
%%
%% @deprecated don't use this function, it's only intended for debugging moka.
-spec stop(moka_server()) -> ok.
stop(MokaServ) -> sel_gen_server:call(MokaServ, stop).

%% @doc Use {@link moka:replace/3}
-spec replace(moka_server(), atom(), fun()) -> ok.
replace(MokaServ, Function, NewBehaviour) ->
    sel_gen_server:call(MokaServ, {replace, {local, Function}, NewBehaviour}).

%% @doc Use {@link moka:replace/4}
-spec replace(moka_server(), module(), atom(), fun()) -> ok.
replace(MokaServ, Module, Function, NewBehaviour) ->
    sel_gen_server:call(
      MokaServ, {replace, {external, Module, Function}, NewBehaviour}).

%% @doc Use {@link moka:export/3}
-spec export(moka_server(), atom(), non_neg_integer()) -> ok.
export(MokaServ, Function, Arity) ->
    sel_gen_server:call(MokaServ, {export, Function, Arity}).

%% @doc Use {@link moka:load/1}
-spec load(moka_server()) -> ok.
load(MokaServ) -> sel_gen_server:call(MokaServ, load).

%%%_* gen_server callbacks =============================================

%% @private
init({Mod, AbsCode}) ->
    try
        %% needed to have terminate run before the server dies
        process_flag(trap_exit, true),
        {ok, #state{
                module   = Mod,
                abs_code = AbsCode
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

safe_handle_call({replace, ReplaceSpec, NewBehaviour}, _From, State) ->
    Count          = State#state.handler_count,
    AbsCode        = State#state.abs_code,
    HandlerName    = start_call_handler(NewBehaviour, Count),
    {arity, Arity} = erlang:fun_info(NewBehaviour, arity),

    {reply, ok,
     State#state{
       handler_count = Count + 1,
       abs_code =
           modify_call_in_code(ReplaceSpec, Arity, HandlerName, AbsCode)}};

safe_handle_call({export, Function, Arity}, _From, State) ->
    AbsCode = State#state.abs_code,
    {reply, ok,
     State#state{abs_code = moka_mod_utils:export(Function, Arity, AbsCode)}};

safe_handle_call(load, _From, State) ->
    moka_mod_utils:load_abs_code(State#state.module, State#state.abs_code),
    {reply, ok, State};

safe_handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

safe_handle_call(Request, _From, _State) ->
    throw({bad_call, Request}).

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, State) ->
    moka_mod_utils:restore_module(State#state.module).

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Private functions ================================================

start_call_handler(Behaviour, Count) ->
    HandlerName = call_handler_name(Count),
    moka_call_handler:start_link(HandlerName, Behaviour),
    HandlerName.

call_handler_name(Count) ->
    list_to_atom(lists:flatten(io_lib:format("moka_call_handler_~p", [Count]))).

modify_call_in_code({external, Mod, Funct}, Arity, HandlerName, AbsCode) ->
    moka_mod_utils:replace_remote_calls(
      {Mod, Funct, Arity},
      call_handler_call(HandlerName),
      AbsCode);
modify_call_in_code({local, Funct}, Arity, HandlerName, AbsCode) ->
    moka_mod_utils:replace_local_calls(
      {Funct, Arity},
      call_handler_call(HandlerName),
      AbsCode).

call_handler_call(Name) -> {moka_call_handler, get_response, [Name, '$args']}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
