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

swap_forms_test_() ->
    Module1 = test_module(),
    Module2 = test_module2(),
    {setup,
     fun() ->
             Forms1 = moka_mod_utils:get_forms(Module1),
             Forms2 = moka_mod_utils:get_forms(Module2),
             {Forms1, Forms2}
     end,
     fun(_) -> ok end,
     fun({Forms1, Forms2}) ->
             {inorder,
              [?_assertEqual({foo, bar}, {Module1:foo(), Module2:bar()})
               , ?_assertError(undef, Module1:bar())
               , ?_assertError(undef, Module2:foo())

               , ?_test(moka_mod_utils:load_forms(Module1, Forms2))
               , ?_test(moka_mod_utils:load_forms(Module2, Forms1))

               , ?_assertEqual({bar, foo}, {Module1:foo(), Module2:bar()})
               , ?_assertError(undef, Module2:bar())
               , ?_assertError(undef, Module1:foo())
              ]}
     end}.

%%%===================================================================
%%% Internals
%%%===================================================================
test_module() -> moka_test_module.

test_module2() -> moka_test_module2.

%% We assume this module doesn't exist
fake_module() -> moka_fake_mod.
