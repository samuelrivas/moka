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
  moka_mod_utils:restore_module(dummy_target_module()),
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

meck_create_destroy_bench(Times) ->
  Self = self(),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> meck_power_cycle(Times) end),
        io:format(
          "~.2f cycles per second~n~.2f millisecs per cycle~n",
          [Times*1000000/Microsecs, Microsecs/(Times*1000)]),
        Self ! done
    end),
  receive
    done -> ok
  end.

unmocked_sequential_call(Times) ->
  Self = self(),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> do_call(Times) end),
        io:format(
          "~.2f cycles per milisec~n~.2f microsecs per cycle~n",
          [Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end.

meck_sequential_call_bench(Times) ->
  Self = self(),
  Target = actual_target_module(),
  meck:new(Target),
  meck:expect(Target, call1, fun(_) -> baz end),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> do_call(Times) end),
        io:format(
          "~.2f cycles per milisec~n~.2f microsecs per cycle~n",
          [Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end,
  meck:unload().

moka_sequential_call_bench(Times) ->
  Self = self(),
  Target = actual_target_module(),
  Moka = moka:start(Target),
  moka:replace(Moka, dest_module, call1, fun(_) -> baz end),
  moka:load(Moka),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> do_call(Times) end),
        io:format(
          "~.2f cycles per milisec~n~.2f microsecs per cycle~n",
          [Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end,
  moka:stop(Moka).

moka_parallel_call_bench(Times, Batch) ->
  Self = self(),
  Target = actual_target_module(),
  Moka = moka:start(Target),
  moka:replace(Moka, dest_module, call1, fun(_) -> baz end),
  moka:load(Moka),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> parallel_do_call(Times, Batch) end),
        io:format(
          "~.2f cycles per milisec~n~.2f microsecs per cycle~n",
          [Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end,
  moka:stop(Moka).

meck_parallel_call_bench(Times, Batch) ->
  Self = self(),
  Target = actual_target_module(),
  meck:new(Target),
  meck:expect(Target, call1, fun(_) -> baz end),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> parallel_do_call(Times, Batch) end),
        io:format(
          "~.2f cycles per milisec~n~.2f microsecs per cycle~n",
          [Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end,
  meck:unload().

unmocked_parallel_call_bench(Times, Batch) ->
  Self = self(),
  spawn_link(
    fun() ->
        {Microsecs, _} = timer:tc(fun() -> parallel_do_call(Times, Batch) end),
        io:format(
          "~.2f cycles per milisec~n~.2f microsecs per cycle~n",
          [Times*1000/Microsecs, Microsecs/Times]),
        Self ! done
    end),
  receive
    done -> ok
  end.

%%%_* Private Functions ================================================

moka_power_cycle(0)     -> ok;
moka_power_cycle(Times) ->
  Moka = moka:start(dummy_target_module()),
  moka:load(Moka),
  moka:stop(Moka),
  moka_power_cycle(Times - 1).

meck_power_cycle(0)     -> ok;
meck_power_cycle(Times) ->
  meck:new(dummy_target_module()),
  meck:unload(dummy_target_module()),
  meck_power_cycle(Times - 1).

parallel_do_call(N, _) when N =< 0-> ok;
parallel_do_call(Times, Batch) ->
  Size = erlang:min(Times, Batch),
  Self = self(),
  Pids = [spawn(
            fun() ->
                Target = actual_target_module(),
                Target:call1(dontcare),
                Self ! done
            end)
          || _ <- lists:seq(1, Size)],
  receive_dones(Pids),
  parallel_do_call(Times - Size, Batch).

receive_dones([]) -> ok;
receive_dones([_|T]) ->
  receive
    done -> receive_dones(T)
  end.

do_call(0) -> ok;
do_call(Times) ->
  Target = actual_target_module(),
  Target:call1(doncare),
  do_call(Times -1).

%% You need this module compiled with debug info in the code path
dummy_target_module() -> eunit.

actual_target_module() -> moka_bench_target.

%% Relevant for moka
dest_module() -> moka_bench_dest.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
