%%% @doc This module is part of the moka_fsm_proper_test
%%%
%%% We mock functions going from this module to other modules.

-module(moka_test_origin_module).

-export([read_file/1]).

read_file(File) -> crashfy:untuple(file:read_file(File)).
