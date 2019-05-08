:- op(200, xfx, at).

print_colored_message(pos(F, L, C), MsgFormat, MsgArgs, Color, Title) :-
    atomic_list_concat(
        ['~w:~w:~w ', Color, Title, ':\x1B[0m ', MsgFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [F, L, C | MsgArgs]).

print_warning(Pos, MsgFormat, MsgArgs) :-
    print_colored_message(Pos, MsgFormat, MsgArgs, '\u001b[33m', 'warning').

print_error(Pos, MsgFormat, MsgArgs) :-
    print_colored_message(Pos, MsgFormat, MsgArgs, '\u001b[31m', 'error').

print_error_and_halt(Pos, MsgFormat, MsgArgs) :-
    print_error(Pos, MsgFormat, MsgArgs),
    halt.

fix_file_path(Path, FinalPath) :-
    unix(args([_, ProgramPath])),
    atom_string(ProgramPath, StringPath),
    split_string(StringPath, "/", "", SubStrings),
    substrings_to_path(SubStrings, FixedPath),
    atomic_list_concat([FixedPath, Path], FinalPath).

substrings_to_path([], "") :- !.
substrings_to_path([_], "") :- !.
substrings_to_path([P | Ps], Path) :-
    substrings_to_path(Ps, PathPart),
    atomic_list_concat([P, '/', PathPart], Path).

tuple_of_list(Xs, T) :-
    list_of_tuple(T, Xs).

list_of_tuple((H, T), [H | Ts]) :-
    list_of_tuple(T, Ts),
    !.
list_of_tuple(X, [X]).

pretty_env(Env) :-
    dict_pairs(Env, _, Pairs),
    pretty_env_helper(Pairs).

pretty_env_helper([]) :- nl, !.
pretty_env_helper(['`types'-_ | T]) :-
    !,
    pretty_env_helper(T).
pretty_env_helper([Name-(_,T,_) | Defs]) :-
    format('~w :: ~w\n', [Name, T]),
    pretty_env_helper(Defs).

unique_list(Xs) :-
    \+ not_unique_list(Xs, _).

not_unique_list(Xs, Repeating) :-
    msort(Xs, Ys),
    not_unique_list_helper(Ys, Repeating).

not_unique_list_helper([X, X | _], X) :- !.
not_unique_list_helper([_ | Xs], X)  :-
    not_unique_list_helper(Xs, X).

print_term_tree(Term) :-
    print_term_tree(Term, 0).
print_term_tree(Term, Indentation) :-
    atomic(Term),
    !,
    tab(Indentation),
    format('~w', [Term]).
print_term_tree(Xs, Indentation) :-
    is_list(Xs),
    !,
    tab(Indentation),
    format('~w\n', [Xs]).
print_term_tree(Term, Indentation) :-
    Term =.. [Functor | Args],
    tab(Indentation),
    format('~w(\n', [Functor]),
    NextIndent is Indentation + 4,
    print_term_trees(Args, NextIndent),
    format('\n', []),
    tab(Indentation),
    write(')').

print_term_trees([], _) :- !.
print_term_trees([T | Ts], Indent) :-
    print_term_tree(T, Indent),
    print_term_trees(Ts, Indent).