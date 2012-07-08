%%% @doc Handle code modifications
%%%
%%% A server like this is created for each {@link moka} to handle code
%%% modifications in the source moked module

-module(moka_mod_handler).

-behaviour(gen_server).

%%%===================================================================
%%% Exports
%%%===================================================================
%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% Types
%%%===================================================================
-record(state, {
         }).

-type mod_handler() :: pid().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new mod handler
-spec start_link() -> {ok, mod_handler()} | ignore | {error, term()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

%% @doc Terminates a mod handler
-spec stop(mod_handler()) -> ok.
stop(CallHandler) -> sel_gen_server:call(CallHandler, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) -> {ok, #state{}}.

%% @private
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
