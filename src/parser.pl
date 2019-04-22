:- [lexer].
% :- op(1200,xfx,==>).

parse_file(FileName, AST) :-
    tokenize_file(FileName, Tokens),
    empty_operator_list(Ops),
    catch(
        phrase(program(AST, Ops), Tokens), 
        error(Format, Args) at Pos, 
        print_error(Pos, FileName, Format, Args)
    ).

program(Program, Operators) -->
    [keyword(defop) at Pos],
    !,
    operator_definition(Pos, Operators, NewOperators),
    program(Program, NewOperators).
program([Expr | Program], Operators) -->
    defexpr(Expr, Operators), 
    rest_of_program(Program, Operators).
rest_of_program([Expr | Exprs], Operators) -->
    defexpr(Expr, Operators),
    !,
    rest_of_program(Exprs, Operators).
rest_of_program([], _) --> [].

defexpr(Definition, Operators) -->
    [keyword(define) at Start],
    !,
    define(Definition, Operators, Start).
defexpr(Definition, Operators) -->
    [keyword(defun) at Start],
    !,
    defun(Definition, Operators, Start).
defexpr(TypeDef, Operators) -->
    [keyword(import) at Start],
    !,
    typedef(TypeDef, Operators, Start).
defexpr(Import, _) -->
    [keyword(import) at Start],
    import(Import, Start).
defexpr(Expr, Operators) -->
    expr(Expr, Operators).

define(define(sig(Name, Type), Val) at Pos, Operators, Pos) -->
    signature(Name, Type, Pos),
    !,
    curly_bracket(Start, Pos),
    bracketed_expr(Val, Operators, Start).
define(_, _, Start) -->
    { throw(error('Invalid value signature in definition', []) at Start) }.

signature(Name, Type, _) -->
    identifier(Name),
    [':' at _],
    type(Type),
    [operator('=') at _].

bracketed_expr(Expr, Operators, _) -->
    expr(Expr, Operators),
    curly_bracket_terminator(Start).

curly_bracket(Position, _) -->
    ['{' at Position],
    !.
curly_bracket(_, Start) -->
    { throw(error('Missing opening curly bracket', []) at Start) }.

curly_bracket_terminator(_) -->
    ['{' at _],
    !.
curly_bracket_terminator(Start) -->
    { throw(error('Curly bracket not terminated properly', []) at Start) }.

% TODO:
expr(IfElse, Operators) -->
    [keyword(if) at Start],
    !,
    if_else(Start, IfElse, Operators).
expr(PMatch, Operators) -->
    [keyword(match) at Start],
    !,
    pattern_matching(Start, PMatch, Operators).
expr(LetDef, Operators) -->
    [keyword(let) at Start],
    !,
    let_definition(Start, LetDef, Operators).
expr(sequence(Head, Tail), Operators) -->
    tuple(Head),
    !,
    expr_seq(Tail, Operators).

unit_literal(unit at Pos) -->
    ['(' at Pos],
    [')' at _].

if_else(_, if(Condition, Consequence, Alternative), Operators) -->
    parser(Condition, Operators),
    if_consequence(Pos, Consequence, Operators),
    if_alternative(Pos, Alternative, Operators),
    !.
if_else(Pos, _, _) -->
    { throw(error('Syntax error in if-then-else expression', []) at Pos) }.

if_consequence(_, Consequence, Ops) -->
    [keyword(then) at _],
    !,
    parser(Consequence, Ops).
if_consequence(Pos, _, _) -->
    { 
        throw(error('Missing "then" in if-then-else expression', []) at Pos) 
    }.

if_alternative(_, Alternative, Ops) -->
    [keyword(else) at _],
    !,
    parser(Alternative, Ops).
if_alternative(Pos, _, _) -->
    {
        throw(error('Missing "else" in if-then-else expression', []) at Pos)
    }.

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
    atomic_list_concat(
        ['Invalid operator associativity type ~w in operator declaration.',
         'Allowed types are left, right, none, left_unary and right_unary'], 
        ErrorMessage
    ),
    throw(error(ErrorMessage, [Assoc]) at Pos).

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
