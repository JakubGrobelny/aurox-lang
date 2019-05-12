:- ensure_loaded(utility).

merge_operators(ops(I0, P0), ops(I1, P1), ops(I2, P2)) :-
    dict_pairs(I0, _, IPairs),
    dict_pairs(P0, _, PPairs),
    put_dict(IPairs, I1, I2),
    put_dict(PPairs, P1, P2).

define_operator_if_needed(Op, Priority, Assoc, Ops, Ops) :-
    is_operator_type(Op, Priority, Assoc, Ops),
    !.
define_operator_if_needed(Op, 10, Assoc, ops(I, P), NOps) :-
    member(Assoc, [left, none, right]),
    \+ get_dict(Op, I, _),
    \+ get_dict(Op, P, _),
    !,
    print_warning(
        pos(unknown, '?', '?'), 
        "Undefined operator ~w, assumed default associativity and priority",
        [Op]
    ),
    update_operators(Op, 10, left, ops(I, P), NOps).

valid_priority(N, _) :-
    between(0, 20, N),
    !.
valid_priority(N, Pos) :-
    throw(
        error('Invalid operator prority ~w in operator declaration', [N]) at Pos
    ).

valid_associativity(Assoc, _) :-
    member(Assoc, [left, right, none, prefix, postfix]),
    !.
valid_associativity(Assoc, Pos) :-
    throw(
        error(
            'Invalid operator associativity type ~w in operator declaration.\c,
             Allowed types are left, right, none, prefix and postfix', 
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
    add_operator(Op, Infix, (Priority, Assoc), NewInfix).
update_operators(
    Op, 
    Priority, 
    UnaryAssoc, 
    ops(Infix, PostPrefix), 
    ops(Infix, NewPostPrefix)
) :-
    add_operator(Op, PostPrefix, (Priority, UnaryAssoc), NewPostPrefix).

add_operator(Op, Ops, Val, NewOps) :-
    get_dict(Op, Ops, _),
    !,
    print_warning(
        pos(unknown, '?', '?'),
        "redefinition of operator ~w",
        [Ops]
    ),
    put_dict(Op, Ops, Val, NewOps).
add_operator(Op, Ops, Val, NewOps) :-
    put_dict(Op, Ops, Val, NewOps).

is_operator_type(Op, Priority, Assoc, ops(Infix, _)) :-
    member(Assoc, [left, right, none]),
    !,
    get_dict(Op, Infix, (Priority, Assoc)).
is_operator_type(Op, Priority, UnaryAssoc, ops(_, PostPrefix)) :-
    !,
    get_dict(Op, PostPrefix, (Priority, UnaryAssoc)).

mark_unary_operator(Op, UnOp) :-
    atomic_list_concat(['`', Op], UnOp).
    