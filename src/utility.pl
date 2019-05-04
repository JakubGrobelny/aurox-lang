:- op(200, xfx, at).

print_colored_message(pos(F, L, C), MsgFormat, MsgArgs, Color, Title) :-
    atomic_list_concat(
        ['~w:~w:~w ', Color, Title, ':\x1B[0m ', MsgFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [F, L, C | MsgArgs]).

print_warning(Pos, MsgFormat, MsgArgs) :-
    print_colored_message(Pos, MsgFormat, MsgArgs, '\u001b[33;', 'warning').

print_error(Pos, MsgFormat, MsgArgs) :-
    print_colored_message(Pos, MsgFormat, MsgArgs, '\u001b[31;', 'error').

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