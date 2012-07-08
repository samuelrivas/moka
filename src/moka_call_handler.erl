%%% @doc Simulate the calls to Moked functions
%%%
%%% Moked functions will be redirected to a call handler that will hold the
%%% specifications coming from {@link moka} calls.

-module(moka_call_handler).

-behaviour(gen_server).

%%%===================================================================
%%% Exports
%%%===================================================================
%% API
-export([start_link/1, set_response_fun/2, get_response/2, stop/1]).

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
-type call_handler_ref() :: call_handler() | atom().

-export_type([call_handler/0, call_handler_ref/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new call handler
-spec start_link(atom()) -> call_handler().
start_link(Name) ->
    crashfy:untuple(gen_server:start_link({local, Name}, ?MODULE, [], [])).

%% @doc Sets the fun used to create the response
-spec set_response_fun(call_handler_ref(), fun()) -> ok.
set_response_fun(CallHandler, Fun) when is_function(Fun) ->
    sel_gen_server:call(CallHandler, {set_response_fun, Fun}).

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
