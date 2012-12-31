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

%%% @doc This module is part of the moka_fsm_proper_test
%%%
%%% We mock functions going from this module to other modules.

-module(moka_fsm_test_orig_module).

%% We use these functions to check the moking behaviour
-export([
         call_to_internal/0,
         call_to_internal/1,
         call_to_internal/2,

         direct_external_call/0,
         direct_external_call/1,
         direct_external_call/2,

         indirect_external_call/0,
         indirect_external_call/1,
         indirect_external_call/2,

         direct_undef_dependency/0,
         direct_undef_dependency/1,
         direct_undef_dependency/2,

         indirect_undef_dependency/0,
         indirect_undef_dependency/1,
         indirect_undef_dependency/2
        ]).

%% These functions are for specific testing purposes
-export([get_history/0]).

%%% Functions to assist testing --------------------------------------

%% The fsm tests mok this function to return the history a moked function can
%% view. Note this is slightly different to directly getting the history from
%% moka, as the moked function runs in a different environment
get_history() -> internal_get_history().

internal_get_history() -> erlang:error(not_moked).

%%% Functions to test moking -----------------------------------------

%% Directly call to an internal function that returns a predefined value
call_to_internal()     -> internal_call().
call_to_internal(A)    -> internal_call(A).
call_to_internal(A, B) -> internal_call(A, B).

%% Directly call to functions in other modules
direct_external_call()     -> moka_fsm_test_dest_module:external_call().
direct_external_call(A)    -> moka_fsm_test_dest_module:external_call(A).
direct_external_call(A, B) -> moka_fsm_test_dest_module:external_call(A, B).

%% Call to an internal function that calls to other modules
indirect_external_call()     -> redirect_to_external().
indirect_external_call(A)    -> redirect_to_external(A).
indirect_external_call(A, B) -> redirect_to_external(A, B).

%% These functions directly depend on an unimplemented function
direct_undef_dependency()     -> moka_fsm_test_dest_module:unimplemented().
direct_undef_dependency(A)    -> moka_fsm_test_dest_module:unimplemented(A).
direct_undef_dependency(A, B) -> moka_fsm_test_dest_module:unimplemented(A, B).

%% These functions indirectly depend on an unimplemented function
indirect_undef_dependency()     -> redirect_to_undef().
indirect_undef_dependency(A)    -> redirect_to_undef(A).
indirect_undef_dependency(A, B) -> redirect_to_undef(A, B).

%%% Private functions ------------------------------------------------

redirect_to_undef()     -> moka_fsm_test_dest_module:unimplemented().
redirect_to_undef(A)    -> moka_fsm_test_dest_module:unimplemented(A).
redirect_to_undef(A, B) -> moka_fsm_test_dest_module:unimplemented(A, B).

redirect_to_external()     -> moka_fsm_test_dest_module:external_call().
redirect_to_external(A)    -> moka_fsm_test_dest_module:external_call(A).
redirect_to_external(A, B) -> moka_fsm_test_dest_module:external_call(A, B).

internal_call()     -> {unmoked, []}.
internal_call(A)    -> {unmoked, [A]}.
internal_call(A, B) -> {unmoked, [A, B]}.
