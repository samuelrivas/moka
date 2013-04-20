%%% Copyright (c) 2013, Samuel Rivas <samuelrivas@gmail.com>
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

%%% @doc Acceptance tests

-module(preserve_cover).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================

preserve_cover_test_() ->
    Module = test_module(),
    {setup,
     fun() ->
             Apps        = sel_application:start_app(moka),
             CoverStatus = start_cover(),

             crashfy:untuple(cover:compile_beam(Module)),

             Module:this_was_called(),
             Analysis = analyse(Module),

             Moka = moka:start(Module),
             moka:replace(Moka, internal_call, fun() -> baz end),
             moka:load(Moka),

             {Apps, CoverStatus, Moka, Analysis}
     end,
     fun({Apps, CoverStatus, Moka, _}) ->
             stop_if_not_stopped(Moka),
             sel_application:stop_apps(Apps),
             restore_cover(CoverStatus)
     end,
     fun({_, _, Moka, Analysis}) ->
             {inorder,
              [
               %% Verify the moka is working and add a call in moked mode.
               ?_assertMatch(baz, Module:this_was_not_called()),

               %% Get out of moked mode, we should not be in cover mode again
               ?_test(moka:stop(Moka)),

               %% Note that we are verifying that calls while in moked mode are
               %% not reported by cover. This is not desired behaviour, but is
               %% intended. The integration with cover to count calls in moked
               %% mode is tricky. We want to fix this eventually but in the
               %% meantime the specified behaviour is that calls in moked mode
               %% are ignored and thus we verify that in this test. See the
               %% README for more info on the integration with cover
               ?_assertMatch(Analysis, analyse(Module))]}
     end}.

%%%_* Private Functions ================================================

start_cover() ->
    case cover:start() of
        {error, {already_started, _}} -> already_started;
        {ok, _}                       -> started_by_us
    end.

restore_cover(already_started) -> ok;
restore_cover(started_by_us)   -> crashfy:untuple(cover:stop()).

test_module() -> preserve_cover_aux.

analyse(Module) -> crashfy:untuple(cover:analyse(Module)).

stop_if_not_stopped(Moka) ->
    try   moka:stop(Moka)
    catch not_found -> ok
    end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
