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

%%% @doc A server to simulate the calls to Moked functions
%%%
%%% Moked functions will be redirected to a call handler that will hold the
%%% specifications created with {@link moka} "replace" calls.

-module(moka_call_handler).

-behaviour(gen_server).

%%%===================================================================
%%% Exports
%%%===================================================================
%% API
-export([start_link/4, get_response/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% Types
%%%===================================================================
-record(state, {
          call_description :: atom(),
          reply_fun        :: fun(),
          history_server   :: moka_history:server()
         }).

-type call_handler()     :: pid().
-type call_description() :: {Module::module(), FunctionName::atom()}.
-type call_handler_ref() :: call_handler() | atom().

-export_type([call_handler/0, call_handler_ref/0, call_description/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new call handler with `Fun' as `reply_fun'
-spec start_link(atom(), call_description(), fun(),
                 moka_history:server()) ->
                        call_handler().
start_link(Name, CallDescription, Fun, HistoryServer) when is_function(Fun) ->
    crashfy:untuple(
      gen_server:start_link(
        {local, Name}, ?MODULE, {CallDescription, Fun, HistoryServer}, [])).

%% @doc Gets the response for a call
%%
%% Moked modules call this function instead of the original destination
%% function.
-spec get_response(call_handler_ref(), [term()]) -> term().
get_response(CallHandler, Args) when is_list(Args) ->
    sel_gen_server:call(CallHandler, {get_response, Args}).

%% @doc Terminates a call handler
-spec stop(call_handler_ref()) -> ok.
stop(CallHandler) -> sel_gen_server:call(CallHandler, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({CallDescription, Fun, HistoryServer}) ->
    {ok, #state{
            call_description = CallDescription,
            reply_fun        = Fun,
            history_server   = HistoryServer}}.

%% @private
handle_call({get_response, Args}, From, State) ->
    proc_lib:spawn_link(
      fun() ->
              Result = erlang:apply(State#state.reply_fun, Args),
              moka_history:add_call(
                State#state.history_server,
                State#state.call_description,
                Args, Result),

              gen_server:reply(From, Result)
      end),
    {noreply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Request, _From, State) ->
    {reply, {error, {bad_call, Request}}, State}.

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
