%%% @doc Unit tests for {@link moka_mod_utils}

-module(moka_mod_utils_eunit).

-include_lib("eunit/include/eunit.hrl").

get_beam_code_test_() ->
    FakeModule = fake_module(),
    [?_assert(is_binary(moka_mod_utils:get_object_code(test_module())))
     , ?_assertThrow(
          {cannot_get_object_code, FakeModule},
          moka_mod_utils:get_object_code(FakeModule))].

get_forms_test_() ->
    FakeModule = fake_module(),
    [?_assert(is_list(moka_mod_utils:get_forms(test_module())))
     , ?_assertThrow(
          {cannot_get_object_code, FakeModule},
          moka_mod_utils:get_forms(FakeModule))
    ].

%%%===================================================================
%%% Internals
%%%===================================================================
test_module() -> moka_test_module.

%% We assume this module doesn't exist
fake_module() -> moka_fake_mod.
