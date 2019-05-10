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

fix_relative_path(FilePath, FileName, FixedPath) :-
    atom_string(FilePath, StringPath),
    split_string(StringPath, "/", "", SubStrings),
    substrings_to_path(SubStrings, StrippedPath),
    atomic_list_concat([StrippedPath, FileName], FixedPath).
    
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

unique_list(Xs) :-
    \+ not_unique_list(Xs, _).

not_unique_list(Xs, Repeating) :-
    msort(Xs, Ys),
    not_unique_list_helper(Ys, Repeating).

not_unique_list_helper([X, X | _], X) :- !.
not_unique_list_helper([_ | Xs], X)  :-
    not_unique_list_helper(Xs, X).

prettify_expr(X, X).
