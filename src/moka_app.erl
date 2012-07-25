%%% @copyright 2012 Samuel Rivas
%%% @doc The app module for Moka
%%%
%%% You should not need anything from this module unless you are developing Moka

-module(moka_app).
-behaviour(application).

%%%_* Exports ==========================================================
-export([start/2, stop/1]).

%%%_* API ==============================================================

%% @private
%% @doc Callback to start the moka application
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
                   {ok, pid()} | {ok, pid(), State::term()} | {error, term()}.
start(_StartType, _StartArgs) -> moka_main_sup:start_link().

%% @private
%% @doc Callback to stop the moka application
-spec stop(term()) -> ok.
stop(_State) -> ok.

%%%_* Private Functions ================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
