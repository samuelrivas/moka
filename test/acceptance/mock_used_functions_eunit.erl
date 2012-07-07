%%% @doc Acceptance tests

-module(mock_used_functions_eunit).

%%% We want to mock accesses to file from this function
-export([copy_file/2]).

-include_lib("eunit/include/eunit.hrl").

can_mock_read_write_test() ->
    Moka = moka:new(?MODULE),
    moka:mock(Moka, file, read_file, fun(_) -> test_bin() end),
    moka:mock(
      Moka, file, write_file,
      fun(_, B) ->
              check_equal(B, test_bin()),
              ok
      end),
    moka:start(Moka),
    copy_file("/this/must/not/exist/anywhere", "/this/is/also/fake"),
    moka:unload(Moka).

%% We want to mock this function and check it works without writing to actual
%% files
copy_file(Orig, Dest) ->
    Bin = cashfy:untuple(file:read_file(Orig)),
    crashfy:untuple(file:write_file(Dest, Bin)).

check_equal(A, A) -> true;
check_equal(A, B) -> erlang:error({not_equal, A, B}).

test_bin() -> <<"Just something to test our fine coffee.">>.
