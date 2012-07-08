%%% @doc This module is part of the moka_mod_utils_eunit test
%%%
%%% We mock functions going from this module to other modules.

-module(moka_test_module).

-export([foo/0]).

foo() -> foo.
