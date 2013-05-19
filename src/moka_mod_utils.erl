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

%%% @doc Functions to manipulate loaded code
%%%
%%% To avoid killing any process with dangling old code, functions in this
%%% module fail with `{processes_using_old_code, Module}' if there are such
%%% processes. The alternative behaviour is letting those dangling processes die
%%% with `killed', which easily leads to very difficult to debug situations. If
%%% you see these errors trying to load new code with functions in this module,
%%% you have to find the reason there are processes dangling with old code and
%%% fix that problem.
%%%
%%% @see code

-module(moka_mod_utils).

-export([get_object_code/1, get_abs_code/1, load_abs_code/2, restore_module/1,
         replace_remote_calls/3, replace_local_calls/3, to_str/1, export/3
         is_cover_compiled/1, get_cover_compiled_code/1]).

-type forms()           :: [erl_parse:abstract_form()].
-type remote_call()     :: {module(), atom(), Args::[term()]}.
-opaque abstract_code() :: forms().

-export_type([abstract_code/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the object code of a loadable module
%%
%% Independently of whether the module is loaded, this function fails if the
%% object module cannot be loaded again (i.e. if the beam file is not in the
%% load path).
%%
%% @throws {cannot_get_object_code, Module}
-spec get_object_code(module()) -> binary().
get_object_code(Module) ->
    case code:get_object_code(Module) of
        {_Mod, Code, _File} -> Code;
        error -> throw({cannot_get_object_code, Module})
    end.

%% @doc Returns the abstract code of a loadable module
%%
%% This function throws `{no_abstract_code, Module}' when the module binary does
%% not contain an `abstract_code' chunk. This is usually because the module was
%% not compiled with `debug_info' or because it was stripped afterwards.
%%
%% It also throws `{cannot_get_object, Module}' if `Module' is not a loadable
%% erlang module (e.g. the name is misspelled, or the code is not compiled).
%%
%% @throws {no_abstract_code, module()}
%%       | {cannot_get_object, module()}
-spec get_abs_code(module()) -> abstract_code().
get_abs_code(Module) ->
    ObjectCode = get_object_code(Module),
    get_object_code_forms(ObjectCode).

%% @doc Substitutes current `Module' with the result of compiling `AbsCode'
%%
%% `AbsCode' will be slightly modified:
%% <ul>
%% <li>The attribute `module' of `AbsCode' is set to `Module'</li>
%% <li>A wild attribute `-moka_orig_module(Module).' is added with the former
%%     module name</li>
%% </ul>
%%
%% @throws {processes_using_old_code, Module}
%%       | {cannot_load_code, {Module, Reason}}
-spec load_abs_code(module(), abstract_code()) -> ok.
load_abs_code(Module, AbsCode) ->
    Code = compile_forms(set_module_name(Module, AbsCode)),
    load_new_code(Module, Code).

%% @doc Restores the original module behaviour.
%%
%% To restore the original behaviour, this function unloads the module and loads
%% it again from the code search path.
%%
%% @throws {processes_using_old_code, Module}
%%       | {cannot_load_code, {Module, Reason}}
-spec restore_module(module()) -> ok.
restore_module(Module) ->
    unload(Module),
    handle_load_result(Module, code:load_file(Module)).

%% @doc Whether `Module' is cover compiled
%%
%% Returns `false' equally for non cover compiled and for non existing modules
-spec is_cover_compiled(module()) -> boolean().
is_cover_compiled(Module) ->
    case cover:is_compiled(Module) of
        {file, _} -> true;
        false     -> false
    end.

%% @doc Returns the cover compiled object code of a cover compiled module
%%
%% This binary can be reloaded with {@link restore_module/2} after loading other
%% versions of the module, and cover will keep functioning and counting coverage
%% after it.
%%
%% <b>Here be dragons:</b> this function uses internal implementation details
%% of cover, use it with caution.
%%
%% @throws {cannot_get_cover_compiled_code, Module}
-spec get_cover_compiled_code(module()) -> binary().
get_cover_compiled_code(Module) ->
    %% FIXME The binary table is an implementation detail of cover we are
    %% (ab)using to be able to restore cover compiled code without resetting
    %% cover and re-cover-compiling the module.
    %% http://xkcd.com/292/
    case ets:lookup(cover_binary_code_table, Module) of
        [{Module, Binary}] -> Binary;
        []                 -> throw({cannot_get_cover_compiled_code, Module})
    end.

%% @doc Returns a pretty printed version of `AbsCode'
-spec to_str(abstract_code()) -> iolist().
to_str(AbsCode) -> erl_prettypr:format(erl_syntax:form_list(AbsCode)).

%% @doc Replaces external function calls in `AbsCode'
%%
%% An element `$args' in the `Args' list of the {@link remote_call()} is
%% replaced by the argument list in the old call.
%%
%% For example, if `AbsCode' represents a module `my_mod' containing next code:
%% ```
%% foo() ->
%%    ...
%%    other_module:bar(X).
%% '''
%% Next call will change `other_module:bar(X)' by
%% `io:format("Args: ~p~n", [X])':
%% ```
%% moka_mod_utils:replace_remote_calls(
%%    {other_module, bar, 1},
%%    {io, format, ["Args: ~p~n", '$args']},
%%    AbsCode)
%% '''
-spec replace_remote_calls(mfa(), remote_call(), abstract_code()) ->
                                  abstract_code().
replace_remote_calls({OldMod, OldFun, Arity}, NewCall, AbsCode) ->
    OldCall = {OldMod, {OldFun, Arity}},
    Filter =
        map_if_node_type(
          application,
          fun(Tree) ->
                  case erl_syntax_lib:analyze_application(Tree) of
                      What when What =:= OldCall -> replace(Tree, NewCall);
                      _Other -> Tree
                  end
          end),
    walk_and_filter(Filter, AbsCode).

%% @doc Replaces internal function calls in `AbsCode'
%%
%% An element `$args' in the `Args' list of the {@link remote_call()} is
%% replaced by the argument list in the old call.
%%
%% For example, if `AbsCode' represents a module `my_mod' containing next code:
%% ```
%% foo() ->
%%    ...
%%    bar(X).
%% '''
%% Next call will change `bar(X)' by
%% `io:format("Args: ~p~n", [X])':
%% ```
%% moka_mod_utils:replace_internal_calls(
%%    {bar, 1},
%%    {io, format, ["Args: ~p~n", '$args']},
%%    AbsCode)
%% '''
-spec replace_local_calls({atom(), byte()}, remote_call(), abstract_code()) ->
                                 abstract_code().
replace_local_calls({OldFun, Arity}, NewCall, AbsCode) ->
    OldCall = {OldFun, Arity},
    Filter =
        map_if_node_type(
          application,
          fun(Tree) ->
                  case erl_syntax_lib:analyze_application(Tree) of
                      What when What =:= OldCall -> replace(Tree, NewCall);
                      _Other -> Tree
                  end
          end),
    walk_and_filter(Filter, AbsCode).

%% @doc Adds an exported function to the list of exported functions of `AbsCode'
%%
%% This will add `Funct/Arity' to the first `-exported' attribute found in
%% `AbsCode'.
%%
%% Note that this function doesn't check whether `Funct/Arity' is defined. If
%% you export an undefined function, the `AbsCode' will not compile, and thus
%% cannot be loaded. This will make {@link load_abs_code/2} fail.
-spec export(atom(), non_neg_integer(), abstract_code()) -> abstract_code().
export(Funct, Arity, AbsCode) ->
    assert_defined_function(Funct, Arity, AbsCode),
    Filter =
        map_if_node_type(
          attribute,
          fun(Tree) ->
                  case attribute_name(Tree) of
                      export -> add_export(Tree, Funct, Arity);
                      _Other -> Tree
                  end
          end),
    walk_and_filter(Filter, AbsCode).

%%%===================================================================
%%% Private Functions
%%%===================================================================
get_object_code_forms(Code) ->
    case beam_lib:chunks(Code, [abstract_code]) of
        {ok, {_Mod, Result}} -> get_abstract_code_forms(Result);
        {error, beam_lib, Reason} -> throw({cannot_get_abstract_code, Reason})
    end.

get_abstract_code_forms([{abstract_code, {Version, Forms}}]) ->
    check_abs_vsn(Version),
    Forms;
get_abstract_code_forms([{abstract_code, no_abstract_code}]) ->
    throw(no_abstract_code).

check_abs_vsn(raw_abstract_v1) -> ok;
check_abs_vsn(Other) -> throw({unsupported_abstract_code_version, Other}).

%% @throws {cannot_compile_forms, [Error | Warning]}
-spec compile_forms(forms()) -> binary().
compile_forms(Forms) ->
    case compile:forms(Forms, [debug_info]) of
        {ok, _ModName, Code} ->
            Code;
        {ok, _ModName, Code, Warnings} ->
            report_warnings(Warnings),
            Code;
        error ->
            throw({cannot_compile_forms, []});
        {error, Errors, Warnings} ->
            throw({cannot_compile_forms, Errors ++ Warnings})
    end.

report_warnings(Warnings) ->
    error_logger:info_report(
      ["Moka found warnings trying to compile new forms"
       | Warnings]).

set_module_name(_NewName, []) ->
    [];
set_module_name(NewName, [{attribute, Line, module, Mod} | T]) ->
    [{attribute, Line, module, NewName},
     {attribute, Line, moka_orig_module, Mod}
     | T];
set_module_name(NewName, [H | T]) ->
    [H | set_module_name(NewName, T)].

load_new_code(Module, Code) ->
    unload(Module),
    handle_load_result(Module, code:load_binary(Module, "no file", Code)).

handle_load_result(Module, {module, Module}) ->
    ok;
handle_load_result(Module, {error, Reason}) ->
    throw({cannot_load_code, {Module, Reason}}).

%% We try to be careful not to kill any process lingering with old code as this
%% can lead to very difficult do debug failing test cases. In case there are
%% lingering processes moka will refuse to load new code
%%
%% Note this is not entirely safe as other processes can reload the old code in
%% parallel before we load the new version
unload(Module) ->
    case code:soft_purge(Module) of
        true -> ok;
        false -> throw({processes_using_old_code, Module})
    end.

replace(ApplicTree, {M, F, Args}) ->
    OldArgs = erl_syntax:list(erl_syntax:application_arguments(ApplicTree)),
    New = erl_syntax:application(
            erl_syntax:atom(M),
            erl_syntax:atom(F),
            [make_arg(Arg, OldArgs) || Arg <- Args]),
    New.

make_arg('$args', OldArgs) -> OldArgs;
make_arg(Arg, _) -> erl_syntax:abstract(Arg).

%% Add Name/Arity to the list of exported functions in a tree representing a
%% "-export[...]" tree
add_export(ExportTree, Name, Arity) ->
    [Args] = erl_syntax:attribute_arguments(ExportTree),
    AttrName = erl_syntax:attribute_name(ExportTree),
    FunSpec = erl_syntax:arity_qualifier(
                erl_syntax:atom(Name), erl_syntax:integer(Arity)),
    erl_syntax:attribute(AttrName, [erl_syntax:cons(FunSpec, Args)]).

walk_and_filter(Filter, Forms) ->
    Tree = erl_syntax:form_list(Forms),
    NewTree = erl_syntax_lib:map(Filter, Tree),
    erl_syntax:revert_forms(NewTree).

%% Create a filter that applies SubsFun in a subtree if it is of type Type
map_if_node_type(Type, SubsFun) ->
    fun(Tree) ->
            case erl_syntax:type(Tree) of
                Type -> SubsFun(Tree);
                _Other -> Tree
            end
    end.

attribute_name(Tree) -> erl_syntax:atom_value(erl_syntax:attribute_name(Tree)).

assert_defined_function(Funct, Arity, AbsCode) ->
    case is_defined_function(Funct, Arity, AbsCode) of
        true  -> ok;
        false -> throw({undefined_function, {Funct, Arity}})
    end.

is_defined_function(Funct, Arity, AbsCode) ->
    Tree = erl_syntax:form_list(AbsCode),
    erl_syntax_lib:fold_subtrees(
      fun (SubTree, Found) ->
              case erl_syntax_lib:analyze_form(SubTree) of
                  {function, {Funct, Arity}} -> true;
                  _                          -> Found
              end
      end,
      false, Tree).
