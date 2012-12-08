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

-export([
         direct_undef_dependency/0,
         direct_undef_dependency/1,
         direct_undef_dependency/2
        ]).

%% This function directly depends on an unimplemented function
direct_undef_dependency()     -> moka_fsm_test_dest_module:unimplemented().
direct_undef_dependency(A)    -> moka_fsm_test_dest_module:unimplemented(A).
direct_undef_dependency(A, B) -> moka_fsm_test_dest_module:unimplemented(A, B).


%%% Private functions ------------------------------------------------

