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

%%% @doc acceptance tests

-module(hold_state_eunit).

%%%_* Exports ==========================================================
-export([code_load/0]).

%%%_* Includes =========================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================
can_base_on_previous_results_test() ->
    Apps = sel_application:start_app(moka),
    Moka = moka:start(?MODULE),
    try
        moka:replace(
          Moka, crypto, rand_uniform,
          fun(_, _) ->
                  case moka:history(Moka) of
                      []                          -> 0;
                      [{_Fun, _Args, Return} | _] -> Return + 1
                  end
          end),
        moka:load(Moka),
        ?MODULE:code_load(),

        %% Now we should be able to have a stateful, but controlled pseudorandom
        %% generator
        ?assertEqual(0, crypto:rand_uniform(0, 10)),
        ?assertEqual(1, crypto:rand_uniform(0, 10)),
        ?assertEqual(2, crypto:rand_uniform(0, 10)),
        ?assertEqual(3, crypto:rand_uniform(0, 10))
    after
        moka:stop(Moka),
        sel_application:stop_apps(Apps)
    end.

can_check_arg_sequence_test() ->
    Apps = sel_application:start_app(moka),
    Moka = moka:start(?MODULE),
    try
        moka:replace(
          Moka, crypto, rand_uniform,
          fun(_, _) ->
                  lists:foldl(
                    fun({_Fun, [Arg], _Return}, Acc) -> Arg + Acc end,
                    0, moka:history(Moka))
          end),
        moka:load(Moka),
        ?MODULE:code_load(),

        %% Now the pseudorandom generator should just sum all previous first
        %% arguments.
        ?assertEqual(0, crypto:rand_uniform(1, 10)),
        ?assertEqual(1, crypto:rand_uniform(2, 10)),
        ?assertEqual(3, crypto:rand_uniform(5, 10)),
        ?assertEqual(8, crypto:rand_uniform(0, 10))
    after
        moka:stop(Moka),
        sel_application:stop_apps(Apps)
    end.

can_count_function_calls_test() ->
    Apps = sel_application:start_app(moka),
    Moka = moka:start(?MODULE),
    try
        moka:replace(Moka, crypto, rand_uniform, fun(_, _) -> 0 end),
        moka:replace(Moka, crypto, rand_bytes, fun(_) -> <<>> end),
        moka:load(Moka),
        ?MODULE:code_load(),

        %% Do some calls
        crypto:rand_uniform(foo, bar),
        crypto:rand_bytes(foo),
        crypto:rand_bytes(foo),
        crypto:rand_uniform(foo, bar),
        crypto:rand_uniform(foo, bar),

        %% Check we can count the calls
        History = moka:history(Moka),
        ?assertEqual(3, length([x || {rand_uniform, _Args, _Rtrn} <- History])),
        ?assertEqual(2, length([x || {rand_bytes  , _Args, _Rtrn} <- History]))
    after
        moka:stop(Moka),
        sel_application:stop_apps(Apps)
    end.

%% Used just lo load the moked code
code_load() -> ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
