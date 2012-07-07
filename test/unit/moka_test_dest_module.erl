%%% @doc This module is part of the moka_fsm_proper_test
%%%
%%% We break the dependencies on this module by avoiding calls from the source
%%% module to it.
%%%
%%% All functions here crash, so is easier to spot mokas that failed to remove
%%% the dependency

-module(moka_test_dest_module).

-export([sum/3, sum/2, square/1]).

sum(_, _) -> crash(?LINE).

sum(_, _, _) -> crash(?LINE).

square(_) -> crash(?LINE).

crash(Line) -> erlang:error({called, ?MODULE, {line, Line}}).
