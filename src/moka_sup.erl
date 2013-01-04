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
%%% @doc Supervisor to hold each moka
%%%
%%% Each moka is held by one instance of this supervisor, along with all the
%%% instantiated call handlers
-module(moka_sup).

-behaviour(supervisor).

%%%_* Exports ==========================================================
-export([start_link/5]).

%% Supervisor callbacks
-export([init/1]).

%%%_* API ==============================================================

%% @doc Starts a supervisor for a new moka
-spec start_link(
        atom(), atom(), moka_history:server(), module(),
        moka_mod_utils:abstract_code()) ->
                        {ok, pid()} | {error, term()}.
start_link(SupName, MokaServerName, MokaHistoryName, MokedModule, AbsCode) ->
    supervisor:start_link(
      {local, SupName}, ?MODULE,
      {MokaServerName, MokaHistoryName, MokedModule, AbsCode}).

%%%_* Exported Internals ================================================

%% @private
init({MokaServerName, MokaHistoryName, MokedModule, AbsCode}) ->
    {ok,
     {supervisor_spec(),
      [moka_spec(MokaServerName, MokaHistoryName, MokedModule, AbsCode),
       history_spec(MokaHistoryName)]}}.

%%%_* Private Functions ================================================

supervisor_spec() ->
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    {one_for_all, MaxRestarts, MaxSecondsBetweenRestarts}.

moka_spec(ServerName, HistoryName, Module, AbsCode) ->
    Restart = permanent,
    Shutdown = 2000,
    MFA = {moka_server, start_link, [ServerName, HistoryName, Module, AbsCode]},
    {moka_server, MFA, Restart, Shutdown, worker, [moka_server]}.

history_spec(Name) ->
    Restart = permanent,
    Shutdown = 2000,
    MFA = {moka_history, start_link, [Name]},
    {moka_history, MFA, Restart, Shutdown, worker, [moka_history]}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
