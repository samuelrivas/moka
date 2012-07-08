%%% @doc Functions to manipulate loaded code
%%%
%%% The functions in this module will not kill any process with dangling old
%%% code. If there are such processes, functions will fail with
%%% `{processes_using_old_code, Module}'. This is because the alternative
%%% behaviour of killing those dangling processes can lead to very difficult to
%%% debug situations. If you see these failures, find the reason there are
%%% processes dangling with old code and fix it.
%%%
%%% @see code

%%% TODO Maybe handing abstract trees (the ones handled by erl_syntax) is easier
%%% than handling the Forms list. At least it seems there is an easy way of
%%% traversing the abstract tree with erl_syntax_lib:fold. Needs some
%%% investigation as the form traversing code is already somewhat tricky and we
%%% don't support all forms ...
-module(moka_mod_utils).

-export([get_object_code/1, get_forms/1, load_forms/2, restore_module/1,
         replace_remote_calls/3, to_str/1]).

-type forms() :: [erl_parse:abstract_form()].
-type remote_call() :: {module(), atom(), [term()]}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the object code of a loadable module
%%
%% Independently of whether the module is loaded, this function fails if the
%% object module cannot be loaded again (i.e. if the beam file is not int the
%% load path)
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
%% @throws {processes_using_old_code, Module}
%%       | {cannot_load_code, {Module, Reason}}
-spec load_forms(module(), forms()) -> ok.
load_forms(Module, Forms) ->
    Code = compile_forms(set_module_name(Module, Forms)),
    load_new_code(Module, Code).

%% @doc Restores the original module behaviour.
%%
%% This function unloads the module and loads it again from the code search path
%%
%% @throws {processes_using_old_code, Module}
%%       | {cannot_load_code, {Module, Reason}}
-spec restore_module(module()) -> ok.
restore_module(Module) ->
    unload(Module),
    handle_load_result(Module, code:load_file(Module)).

%% @doc Returns a pretty printed version of `Forms'
-spec to_str(forms()) -> iolist().
to_str(Forms) -> erl_prettypr:format(erl_syntax:form_list(Forms)).

%% @doc Replaces external function calls in `Forms'
%%
%% For example, if `Forms' represents a module `my_mod' containing next code:
%% ```
%% foo() ->
%%    other_module:bar().
%% '''
%% Next call will change `other_module:bar()' by `io:format("Hi there~n")':
%% ```
%% moka_mod_utils:replace_remote_calls(
%%    {other_module, bar, 0}, {io, format, ["Hi there~n"]}, Forms)
%% '''
%%
%% @throws {processes_using_old_code, Module}
%%       | {cannot_load_code, {Module, Reason}}

-define(atom_match(Atom), {atom, _, Atom}).
-define(remote_call_match(Mod, Fun),
        {call, _, {remote, _, ?atom_match(Mod), ?atom_match(Fun)}, _}).

-spec replace_remote_calls(mfa(), remote_call(), forms()) -> ok.
replace_remote_calls({OldMod, OldFun, Arity}, NewCall, Forms) ->
    walk_and_filter(
      fun(Call) ->
              case Call of
                  ?remote_call_match(OldMod, OldFun) ->
                      replace_if_arity_match(Call, Arity, NewCall);
                  _ ->
                      no_match
              end
      end,
      Forms).

replace_if_arity_match(Call, Arity, NewCall) ->
    case call_arity(Call) of
        Arity -> remote_call_form(NewCall);
        _Other -> Call
    end.

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
    handle_load_result(Module, code:load_binary(Module, "no file", Code)).

handle_load_result(Module, {module, Module}) ->
    ok;
handle_load_result(Module, {error, Reason}) ->
    throw({cannot_load_code, {Module, Reason}}).

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

walk_and_filter(_Filter, []) ->
    [];
walk_and_filter(Filter, [H | T]) ->
    [step(Filter, H) | walk_and_filter(Filter, T)];
walk_and_filter(Filter, Form) ->
    step(Filter, Form).

step(Filter, Form) ->
    case Filter(Form) of
        no_match -> walk_next(Filter, Form);
        NewForm -> NewForm
    end.

call_arity({call, _Line, _Left, Args}) -> length(Args).

remote_call_form({Module, Function, Args}) ->
    {call, 0, {remote, 0, to_form(Module), to_form(Function)},
     [to_form(X) || X <- Args]}.

to_form(Term) -> erl_parse:abstract(Term).

%%%-------------------------------------------------------------------
%%% Functions to walk the forms list
%%%-------------------------------------------------------------------
%% Keep this function from default matching, We want to know when something
%% unsupported slips through for now.
%%
%% A decent quickcheck test might help to avoid this

%% Terminal Forms
walk_next(_Filter, Form = {attribute, _, _, _}) -> Form;
walk_next(_Filter, Form = {atom, _, _}) -> Form;
walk_next(_Filter, Form = {eof, _}) -> Form;

%% Non Terminal Forms
walk_next(Filter, {function, Line, Name, Arity, Body}) ->
    {function, Line, Name, Arity, walk_and_filter(Filter, Body)};
walk_next(Filter, {remote, Line, Left, Right}) ->
    {remote, Line,
     walk_and_filter(Filter, Left),
     walk_and_filter(Filter, Right)};
walk_next(Filter, {clause, Line, Pattern, Guards, Body}) ->
    {clause, Line,
     walk_and_filter(Filter, Pattern),
     walk_and_filter(Filter, Guards),
     walk_and_filter(Filter, Body)};
walk_next(Filter, {call, Line, Body, Args}) ->
    {call, Line,
     walk_and_filter(Filter, Body),
     walk_and_filter(Filter, Args)}.
