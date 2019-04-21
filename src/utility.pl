print_error(pos(L, C), FileName, MessageFormat, MessageArgs) :-
    atomic_list_concat(
        ['~w:~w:~w \u001b[31;1merror:\x1B[0m ', MessageFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [FileName, L, C | MessageArgs]),
    halt.

get_nth([X | Xs], 0, X, Xs) :-
    !.
get_nth([X | Xs], N, Y, [X | Ys]) :-
    !,
    M is N - 1,
    get_nth(Xs, M, Y, Ys).
get_nth([], _, _, _) :-
    throw(out_of_bounds_error).

put_nth(Xs, 0, X, [X | Xs]) :-
    !.
put_nth([X | Xs], N, Elem, [X | Ys]) :-
    !,
    M is N - 1,
    put_nth(Xs, M, Elem, Ys).
put_nth([], _, _, _) :-
    throw(out_of_bounds_error).

