:- ensure_loaded(utility).

replace_functor([], _, []) :- !.
replace_functor([X | Xs], F, [Y | Ys]) :-
    X =.. [_ | Args],
    Y =.. [F | Args],
    replace_functor(Xs, F, Ys).

merge_operators(ops(I0, P0), ops(I1, P1), ops(I2, P2)) :-
    dict_pairs(I0, _, IPairs),
    dict_pairs(P0, _, PPairs),
    replace_functor(IPairs, ':', IPairsReplaced),
    replace_functor(PPairs, ':', PPairsReplaced),
    put_dict(IPairsReplaced, I1, I2),
    put_dict(PPairsReplaced, P1, P2).

define_operator_if_needed(Op, Priority, Assoc, Ops, Ops) :-
    is_operator_type(Op, Priority, Assoc, Ops),
    !.
define_operator_if_needed(Op, 10, Assoc, ops(I, P), NOps) :-
    member(Assoc, [left, none, right]),
    \+ get_dict(Op, I, _),
    \+ get_dict(Op, P, _),
    !,
    update_operators(Op, 10, left, ops(I, P), NOps).

valid_priority(N, _) :-
    between(0, 20, N),
    !.
valid_priority(N, Pos) :-
    throw(
        error('Invalid operator prority ~w in operator declaration', [N]) at Pos
    ).

valid_associativity(Assoc, _) :-
    member(Assoc, [left, right, none, left_unary, right_unary]),
    !.
valid_associativity(Assoc, Pos) :-
    throw(
        error(
            'Invalid operator associativity type ~w in operator declaration.\c,
             Allowed types are left, right, none, left_unary and right_unary', 
            [Assoc]
        ) at Pos
    ).

empty_operator_list(ops(Infix, PostPrefix)) :-
    dict_create(Infix, infix, []),
    dict_create(PostPrefix, postprefix, []).

infix(Assoc) :-
    member(Assoc, [left, right, none]).

update_operators(
    Op, 
    Priority, 
    Assoc, 
    ops(Infix, PostPrefix), 
    ops(NewInfix, PostPrefix)
) :-
    infix(Assoc),
    !,
    put_dict(Op, Infix, (Priority, Assoc), NewInfix).
update_operators(
    Op, 
    Priority, 
    UnaryAssoc, 
    ops(Infix, PostPrefix), 
    ops(Infix, NewPostPrefix)
) :-
    put_dict(Op, PostPrefix, (Priority, UnaryAssoc), NewPostPrefix).

is_operator_type(Op, Priority, Assoc, ops(Infix, _)) :-
    member(Assoc, [left, right, none]),
    !,
    get_dict(Op, Infix, (Priority, Assoc)).
is_operator_type(Op, Priority, UnaryAssoc, ops(_, PostPrefix)) :-
    !,
    get_dict(Op, PostPrefix, (Priority, UnaryAssoc)).

