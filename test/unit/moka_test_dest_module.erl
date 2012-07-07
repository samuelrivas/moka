%%% @doc This module is part of the moka_fsm_proper_test
%%%
%%% We break the dependencies on this module by avoiding calls from the source
%%% module to it

-module(moka_test_dest_module).

-export([sum/2]).

sum(A, B) -> A + B.
