%%% @doc Functions to manipulate loaded code

-module(moka_mod_utils).

-export([get_object_code/1, get_forms/1, load_forms/2]).

-type forms() :: [erl_parse:abstract_form()].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the object code of a loadable module
%%
%% Independently of whether the module is loaded, this function fails if the
%% object module be loaded (i.e. if the beam file is not int the load path)
%%
%% @throws {cannot_get_object_code, Module}
-spec get_object_code(module()) -> binary().
get_object_code(Module) ->
    case code:get_object_code(Module) of
        {_Mod, Code, _File} -> Code;
        error -> throw({cannot_get_object_code, Module})
    end.

%% @doc Returns the abstract form of a loadable module
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
-spec get_forms(module()) -> forms().
get_forms(Module) ->
    ObjectCode = get_object_code(Module),
    get_object_code_forms(ObjectCode).

%% @doc Substitutes current `Module' with the result of compiling `Forms'
%%
%% `Forms' will be slightly modified:
%% <ul>
%% <li>The attribute `module' of `Forms' is set to `Module'</li>
%% <li>A wild attribute `-moka_orig_module(Module).' is added with the former
%%     module name</li>
%% </ul>
%%
%% If there are processes using old code this function throws
%% `{processes_using_old_code, Module}'. No process will be killed to substitute
%% the code. This is because the alternative behaviour of killing those dangling
%% processes can lead to very difficult to debug situations. If a test case
%% fails because of this, find the reason there are processes dangling with old
%% code and fix it.
%%
%% @throws {processes_using_old_code, Module}
%%       | {cannot_load_code, {Module, Reason}}
-spec load_forms(module(), forms()) -> ok.
load_forms(Module, Forms) ->
    Code = compile_forms(set_module_name(Module, Forms)),
    load_new_code(Module, Code).

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
    [{attribute, Line, module, NewName}
     , {attribute, Line, moka_orig_module, Mod}
     | T];
set_module_name(NewName, [H | T]) ->
    [H | set_module_name(NewName, T)].

load_new_code(Module, Code) ->
    unload(Module),
    case code:load_binary(Module, "no file", Code) of
        {module, Module} -> ok;
        {error, Reason} -> throw({cannot_load_code, {Module, Reason}})
    end.

%% We try to be careful not to kill any process lingering with old code as this
%% can lead to very difficult do debug failing test cases. In case there are
%% lingering processes moka will refuse to load new code
unload(Module) ->
    safe_purge(Module),
    delete(Module).

safe_purge(Module) ->
    case code:soft_purge(Module) of
        true -> ok;
        false -> throw({processes_using_old_code, Module})
    end.

delete(Module) ->
    case code:delete(Module) of
        true -> ok;
        false ->
            %% This is either a bug or a concurrency problem
            erlang:error({cannot_delete_code, Module})
    end.
