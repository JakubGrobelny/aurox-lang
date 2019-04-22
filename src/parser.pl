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
    [keyword(type) at Start],
    !,
    typedef(TypeDef, Operators, Start).
defexpr(Import, _) -->
    [keyword(import) at Start],
    import(Import, Start).
defexpr(Expr, Operators) -->
    expr(Expr, Operators).

typedef(typedef(Name, Parameters, Constructors), _, Start) -->
    typedef_name(Name, Start),
    typedef_params(Parameters, Start),
    curly_bracket(BracketStart, Start),
    typedef_constructors(Constructors, Start),
    curly_bracket_terminator(BracketStart).

typedef_constructors([constructor(Name, Type) | Constructors], Start) -->
    [tid(Name) at _],
    !,
    typedef_constructor_type(Type),
    typedef_constructors(Constructors, Start).
typedef_constructors([]) --> [].

typedef_constructor_type(Type) -->
    ['{' at BracketStart],
    !,
    type(Type at _),
    curly_bracket_terminator(BracketStart).
typedef_constructor_type([]) --> [].

typedef_params(Params, Start) -->
    [':' at _],
    !,
    typedef_params_list(Params, Start).
typedef_params([], _) --> [].

typedef_params_list([Param | Tail], _) -->
    [id(Param) at _],
    typedef_params_list_tail(Tail).
typedef_params_list(_, Start) -->
    { throw(error('Invalid type parameter', []) at Start) }.
typedef_params_list_tail([Param | Tail], _) -->
    [id(Param) at _],
    typedef_params_list_tail(Tail).
typedef_params_list_tail([]) --> [].

typedef_name(Name, _) -->
    [tid(Name) at _],
    !.
typedef_name(_, Start) -->
    { throw(error('Invalid type name', []) at Start) }.

defun(defun(Name, Args, Type, Body) at Pos, Operators, Pos) -->
    argument_sequence([Name | Args]), 
    !,
    defun_rest(Type, Body, Pos, Operators).
defun(_, _, Start) -->
    { throw(error('Missing name in function definition', []) at Start) }.

defun_rest(Type, Body, Start, Operators) -->
    [':' at _],
    type(Type),
    ['=' at _],
    !,
    curly_bracket(BracketStart, Start),
    expr(Body, Operators),
    curly_bracket_terminator(BracketStart).
defun_rest(_, _, Start, _) -->
    { throw(error('Missing ":=" in function definition', []) at Start) }.

argument(Arg) -->
    ['(' at _],
    !,
    [operator(Arg) at _],
    [')' at _].
argument(Arg) -->
    [id(Arg)].

argument_sequence([Arg | Tail]) -->
    argument(Arg),
    argument_sequence_tail(Tail).
argument_sequence_tail([Arg | Tail]) -->
    argument(Arg),
    !,
    argument_sequence_tail(Tail).
argument_sequence_tail([]) --> [].

import(import(Files), Start) -->
    curly_bracket(_, Start),
    !,
    files_to_import(Files),
    curly_bracket_terminator(Start).

files_to_import([File at Pos | Files]) -->
    [tid(File) at Pos],
    !,
    files_to_import(Files).
files_to_import([File at Pos | Files]) -->
    [string(File) at Pos],
    !,
    files_to_import(Files).
files_to_import([]) --> [].

define(define(Sig, Val) at Pos, Operators, Pos) -->
    signature(Sig),
    !,
    curly_bracket(Start, Pos),
    bracketed_expr(Val, Operators, Start).
define(_, _, Start) -->
    { throw(error('Invalid value signature in definition', []) at Start) }.

signature(sig(Name, Type)) -->
    [id(Name) at _],
    [':' at _],
    !,
    signature_tail(Type),
    [operator('=') at _].
signature_tail([]) -->
    [operator('=') at _],
    !.
signature_tail(Type) -->
    type(Type),
    [operator('=') at _].

bracketed_expr(Expr, Operators, Start) -->
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
    expr(Condition, Operators),
    if_consequence(Pos, Consequence, Operators),
    if_alternative(Pos, Alternative, Operators),
    !.
if_else(Pos, _, _) -->
    { throw(error('Syntax error in if-then-else expression', []) at Pos) }.

if_consequence(_, Consequence, Ops) -->
    [keyword(then) at _],
    !,
    expr(Consequence, Ops).
if_consequence(Pos, _, _) -->
    { 
        throw(error('Missing "then" in if-then-else expression', []) at Pos) 
    }.

if_alternative(_, Alternative, Ops) -->
    [keyword(else) at _],
    !,
    expr(Alternative, Ops).
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

type(Type) -->
    function_type(Left at Start),
    type_tail(Left, Type, Start).
type_tail(Left, tuple_type(Left, Right) at Start, Start) -->
    [',' at _],
    !,
    type(Right).
type_tail(Left, Left) --> [].

function_type(Type) -->
    algebraic_type(Left),
    function_type_tail(Left, Type).
function_type_tail(Left, function_type(Left, Right) at Pos) -->
    [operator('->') at Pos],
    !,
    function_type(Right).
function_type_tail(Left, Left) --> [].

algebraic_type(Atomic) -->
    atomic_type(Atomic),
    !.
algebraic_type(adt(Type, Parameters) at Pos) -->
    [tid(Type) at Pos],
    atomic_type_sequence(Parameters).

atomic_type_sequence([Head | Tail]) -->
    atomic_type(Head),
    !,
    atomic_type_sequence(Tail).
atomic_type_sequence([]) --> [].

atomic_type(param(Id) at Pos) -->
    [id(Id) at Pos],
    !.
atomic_type(type_name(Name) at Pos) -->
    [tid(Name) at Pos],
    !.
atomic_type(list(Type) at Start) -->
    ['[' at Start],
    !,
    type(Type at _),
    [']' at _].
atomic_type(Type) -->
    ['(' at _],
    !,
    type(Type),
    [')' at _].

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
