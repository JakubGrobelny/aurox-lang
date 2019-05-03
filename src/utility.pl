:- op(200, xfx, at).

print_error(pos(FileName, L, C), MessageFormat, MessageArgs) :-
    atomic_list_concat(
        ['~w:~w:~w \u001b[31;1merror:\x1B[0m ', MessageFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [FileName, L, C | MessageArgs]).

print_warning(pos(FileName, L, C), MessageFormat, MessageArgs) :-
    atomic_list_concat(
        ['~w:~w:~w \u001b[33;1mwarning:\x1B[0m ', MessageFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [FileName, L , C | MessageArgs]).

print_error_and_halt(Pos, MsgFormat, MsgArgs) :-
    print_error(Pos, MsgFormat, MsgArgs),
    halt.

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