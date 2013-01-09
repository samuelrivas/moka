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
%%%
%%% Note the `replace' functions replace the function calls, not the functions
%%% themselves. Thus you can still call the original function in the target
%%% module after using {@link replace/4} to substitute the calls from the moked
%%% module. Also, if you export a function with {@link export/3} you always will
%%% have access to the original behaviour, even if you have substituted all
%%% internal calls to it with {@link replace/3}.

-module(moka).

%%%===================================================================
%%% Exports
%%%===================================================================

-export([start/1, stop/1, replace/3, replace/4, export/3, history/1,
         load/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-opaque moka() :: atom().

-type history_entry() :: {Funct::atom(), Args::[any()], Return::any()}.
-type history()       :: [history_entry()].

-export_type([moka/0, history/0, history_entry/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts a new moka
%%
%% `Mod' will be know as <b>the moked module</b> in this documentation. The
%% created Moka will be used to modify calls from this module.
%%
%% <b>NOTE:</b> You should never create two Mokas for the same module.
%%
%% @todo Avoid the possibility of creating two mokas for the same module.
-spec start(module()) -> moka().
start(Mod) when is_atom(Mod) ->
    AbsCode         = moka_mod_utils:get_abs_code(Mod),
    MokaServerName  = moka_server_name(Mod),
    MokaHistoryName = moka_history_name(Mod),

    moka_main_sup:start_moka(MokaServerName, MokaHistoryName, Mod, AbsCode),
    MokaServerName.

%% @doc Stops an existing moka
%%
%% The original module code is restored after stopping the moka.
-spec stop(moka()) -> ok.
stop(Moka) -> moka_main_sup:stop_moka(Moka).

%% @doc Substitutes all calls to `Function' in the moked module
%%
%% The arity of `NewBehaviour' determines the arity of the substituted function.
%%
%% @todo Errors when there are no calls to the substituted function.
-spec replace(moka(), atom(), fun()) -> ok.
replace(Moka, Function, NewBehaviour)
  when is_atom(Function), is_function(NewBehaviour) ->
    moka_server:replace(Moka, Function, NewBehaviour).

%% @doc Substitutes all calls from the moked module to `Mod:Fun/N'
%%
%% The arity of `NewBehaviour' determines the arity of the substituted function.
%%
%% @todo Errors when there are no calls to the substituted function.
-spec replace(moka(), module(), atom(), fun()) -> ok.
replace(Moka, Module, Function, NewBehaviour)
  when is_atom(Module), is_atom(Function), is_function(NewBehaviour) ->
    moka_server:replace(Moka, Module, Function, NewBehaviour).

%% @doc Adds `Fun/Arity' to the list of exported functions of the moked module
%%
%% Note that this doesn't check if exporting `Fun/Arity' is possible, so it will
%% succeed for any atom-integer pair. {@link load/1} will fail if exporting
%% `Fun/Arity' is not possible (e.g. if that function is undefined).
-spec export(moka(), atom(), non_neg_integer()) -> ok.
export(Moka, Function, Arity)
  when is_atom(Function), is_integer(Arity) ->
    moka_server:export(Moka, Function, Arity).

%% @doc Returns the history of calls for `Moka'
%%
%% This function can be called from replaced calls
-spec history(moka()) -> history().
history(Moka) -> moka_server:history(Moka).

%% @doc Makes all substitutions effective
%%
%% The moked module code will be changed after this function is called.
%%
%% You can easily know when a module is moked calling
%% `Module:module_info(attributes)'. For moked modules, the returned list
%% contains a `moka_orig_module' tuple.
-spec load(moka()) -> ok.
load(Moka) -> moka_server:load(Moka).

%%%===================================================================
%%% Internal functions
%%%===================================================================
moka_server_name(Module) ->
    moka_utils:atom_append(atom_to_list(Module), "_moka").

moka_history_name(Module) ->
    moka_utils:atom_append(atom_to_list(Module), "_moka_history").
