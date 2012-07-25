%%% @copyright 2012 Samuel Rivas
%%% @doc Utility functions for Moka internal uses
-module(moka_utils).

%%%_* Exports ==========================================================
-export([atom_append/2]).

%%%_* API ==============================================================

%% @doc Creates an atom concatenating two strings
-spec atom_append(string(), string()) -> atom().
atom_append(Str1, Str2) -> list_to_atom(Str1 ++ Str2).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
