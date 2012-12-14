%%% @doc Test moka performance
%%%
%%% We use meck as baseline for comparison purposes
-module(moka_bench).

%%%_* Exports ==========================================================
-compile(export_all).
-export([]).

%%%_* Includes =========================================================

%%%_* Types ============================================================

%%%_* API ==============================================================
moka_create_destroy_bench(Times) ->
  sel_application:start_app(moka),
  Self = self(),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> moka_power_cycle(Times) end),
        io:format(
          "~.2f cycles per second~n~.2f millisecs per cycle~n",
          [Times*1000000/Microsecs, Microsecs/(Times*1000)]),
        Self ! done
    end),
  receive
    done -> ok
  end.


%%%_* Private Functions ================================================

moka_power_cycle(0)     -> ok;
moka_power_cycle(Times) ->
  Moka = moka:start(target_module()),
  moka:load(Moka),
  moka:stop(Moka),
  moka_power_cycle(Times - 1).

%% You need this module compiled with debug info in the code path
target_module() -> eunit.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
