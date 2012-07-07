%%% @doc This module is part of the moka_fsm_proper_test
%%%
%%% We mock functions going from this module to other modules.

-module(moka_test_origin_module).

-export([sum/2, sum/3, square/1, unimplemented/0]).

sum(A, B) -> moka_test_dest_module:sum(A, B).

sum(A, B, C) -> moka_test_dest_module:sum(A, B, C).

square(A) -> moka_test_dest_module:square(A).

%% This calls a function that doesn't exist
unimplemented() -> moka_test_dest_module:unimplemented().
