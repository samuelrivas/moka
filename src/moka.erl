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

%%% @doc The moka main interface
%%%
%%% This module implements the operations to create new mokas and use them to
%%% mock calls from one module to other.

-module(moka).

%%%===================================================================
%%% Exports
%%%===================================================================

-export([start/1, stop/1, replace/4, load/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-opaque moka() :: atom().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new moka process
%%
%% `Mod' will be know as <b>the moked module</b> in this documentation. The
%% created Moka will be used to modify calls from this module.
%%
%% <b>NOTE:</b> You should never create two Mokas for the same module
%%
%% @todo Avoid the possibility of creating two mokas for the same module
-spec start(module()) -> moka().
start(Mod) ->
    MokaName = moka_name(Mod),
    SupName = moka_sup_name(Mod),
    case moka_sup:start_link(SupName, MokaName, Mod) of
        {ok, _Pid} -> MokaName;
        {error, Reason} -> throw({cannot_start_moka_sup, Reason})
    end.

%% FIXME Implement this using the global supervisor
%% @doc Stops a moka process
-spec stop(moka()) -> ok.
stop(_Moka) -> erlang:error(unimplemented).

%% @doc Substitutes all calls from the moked module to `Mod:Fun/N'
%%
%% The arity of `NewBehaviour' determines the arity of the substituted function
%%
%% @todo Errors when there are no calls to the substituted function
-spec replace(moka(), module(), atom(), fun()) -> ok.
replace(Moka, Module, Function, NewBehaviour) ->
    sel_gen_server:call(Moka, {replace, Module, Function, NewBehaviour}).

%% @doc Makes all substitutions effective
%%
%% The arity of `NewBehaviour' determines the arity of the substituted function
%%
%% @todo Errors when there are no calls to the substituted function
-spec load(moka()) -> ok.
load(Moka) -> sel_gen_server:call(Moka, load).

%%%===================================================================
%%% Internal functions
%%%===================================================================
moka_name(Module) -> atom_append(atom_to_list(Module), "_moka").
moka_sup_name(Module) -> atom_append(atom_to_list(Module), "_moka_sup").

atom_append(Str1, Str2) -> list_to_atom(Str1 ++ Str2).
