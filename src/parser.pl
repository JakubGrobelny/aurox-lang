:- [lexer].
% :- op(1200,xfx,==>).

parse_file(FileName, AST) :-
    tokenize_file(FileName, Tokens),
    empty_operator_list(Ops),
    catch(
        phrase(parser(AST, Ops), Tokens), 
        error(Format, Args) at Pos, 
        print_error(Pos, FileName, Format, Args)
    ).

parser(Tokens, Operators) -->
    [keyword(defop) at Pos],
    !,
    operator_definition(Pos, Operators, NewOperators),
    parser(Tokens, NewOperators).
parser([], _) --> [].

operator_definition(_, PrevOperators, NewOperators) -->
    ['{' at _],
    [operator(Op) at _],
    [integer(Priority) at PriorityPos],
    [id(Associativity) at AssocPos],
    ['}' at _],
    {
        valid_priority(Priority, PriorityPos),
        valid_associativity(Associativity, AssocPos),
        update_operators(
            Op, 
            Priority, 
            Associativity, 
            PrevOperators, 
            NewOperators
        )
    },
    !.
operator_definition(Pos, _, _) -->
    { throw(error('Syntax error in operator definition', []) at Pos) }.

valid_priority(integer(N), _) :-
    member(N, [0,1,2,3,4,5]),
    !.
valid_priority(integer(N), Pos) :-
    throw(
        error('Invalid operator prority ~w in operator declaration', [N]) at Pos
    ).

valid_associativity(id(Assoc), _) :-
    member(Assoc, [left, right, none, left_unary, right_unary]),
    !.
valid_associativity(id(Assoc), Pos) :-
    throw(
        error(
            'Invalid operator associativity type ~w in operator declaration. \c
             Allowed types are left, right, none, left_unary and right_unary', 
            [Assoc]
        ) at Pos
    ).

empty_operator_list(
        ops([[[], [], [], [], [], []],
             [[], [], [], [], [], []],
             [[], [], [], [], [], []],
             [[], [], [], [], [], []],
             [[], [], [], [], [], []]])
).

translate_assoc(left, 0) :- 
    !.
translate_assoc(right, 1) :- 
    !.
translate_assoc(none, 2) :-
    !.
translate_assoc(left_unary, 3) :- 
    !.
translate_assoc(right_unary, 4).

update_operators(Op, Priority, Assoc, ops(Ops), ops(NewOps)) :-
    translate_assoc(Assoc, AssocNum),
    get_nth(Ops, AssocNum, OpsWithAssoc, RestAssocs),
    get_nth(OpsWithAssoc, Priority, OpsWithPriority, RestPriority),
    select(Op, NewOpsWithPriority, OpsWithPriority),
    put_nth(RestPriority, Priority, NewOpsWithPriority, NewOpsWithAssoc),
    put_nth(RestAssocs, AssocNum, NewOpsWithAssoc, NewOps).
