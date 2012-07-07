%%% @doc Simulate the calls to mocked functions
%%%
%%% Mocked functions will be redirected to a call handler that will hold the
%%% specifications coming from {@link mocka} calls.

-module(moka_call_handler).

-behaviour(gen_server).

%%%===================================================================
%%% Exports
%%%===================================================================
%% API
-export([start_link/0, set_response_fun/2, get_response/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% Types
%%%===================================================================
-record(state, {
          reply_fun = undefined :: fun()
         }).

-type call_handler() :: pid().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new call handler
-spec start_link() -> {ok, call_handler()} | ignore | {error, term()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

%% @doc Sets the fun used to create the response
-spec set_response_fun(call_handler(), fun()) -> ok.
set_response_fun(CallHandler, Fun) when is_function(Fun) ->
    sel_gen_server:call(CallHandler, {set_response_fun, Fun}).

%% @doc Gets the response for a call
%%
%% Mocked modules call this function instead of the original destination
%% function.
-spec get_response(call_handler(), [term()]) -> term().
get_response(CallHandler, Args) when is_list(Args) ->
    sel_gen_server:call(CallHandler, {get_response, Args}).

%% @doc Terminates a call handler
-spec stop(call_handler()) -> ok.
stop(CallHandler) -> sel_gen_server:call(CallHandler, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) -> {ok, #state{}}.

%% @private
handle_call({set_response_fun, Fun}, _From, State) ->
    {reply, ok, State#state{reply_fun = Fun}};
handle_call({get_response, Args}, _From, State) ->
    {reply, erlang:apply(State#state.reply_fun, Args), State};
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
