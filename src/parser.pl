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
    signature_tail(Type).
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
expr(LetDef at Start, Operators) -->
    [keyword(let) at Start],
    !,
    let_definition(Start, LetDef, Operators).
expr(sequence(Head, Tail), Operators) -->
    tuple(Head),
    !,
    expr_seq(Tail, Operators).

% TODO: tuple/3

let_definition(Start, let_definition(Sig, Val, In) at Start, Ops) -->
    signature(Sig),
    expr(Val, Ops),
    !,
    let_definition_in_expr(Start, In, Ops).
let_definition(Start, _, _) -->
    { throw(error('Ill formed let definition', []) at Start) }.

let_definition_in_expr(Start, In, Ops) -->
    [keyword(in) at _],
    !,
    curly_bracket(CurlyBracketStart, Start),
    bracketed_expr(In, Ops, CurlyBracketStart).
let_definition_in_expr(Start, _, _) -->
    { throw(error('Missing in keyword in let definition', []) at Start) }.

expr_seq([Expr | Exprs], Operators) -->
    [';' at _],
    !,
    expr(Expr, Operators),
    expr_seq(Exprs, Operators).
expr_seq([], _) --> [].

pattern_matching(Start, pmatch(Expr, Patterns) at Start, Operators) -->
    expr(Expr, Operators),
    !,
    curly_bracket(BracketStart, _),
    pattern_list(Patterns, Operators),
    curly_bracket_terminator(BracketStart).
pattern_matching(Start, _, _) -->
    { throw(error('Invalid pattern matching expression', []) at Start) }.

pattern_list([Pattern at Start | Patterns], Operators) -->
    [keyword(case) at Start],
    !,
    pattern_case(Pattern, Start, Operators),
    pattern_list(Patterns, Operators).
pattern_list([]) --> [].

pattern_case(pattern_case(Pattern, Expr) at Pos, Pos, Operators) -->
    guarded_pattern(Pattern, Pos),
    !,
    pattern_arrow(Pos),
    pattern_expr(Expr, Pos, Operators).
pattern_case(_, Pos, _) -->
    { throw(error('Invalid pattern in pattern matching case', []) at Pos) }.

pattern_arrow(_) -->
    [operator('=>') at _],
    !.
pattern_arrow(Pos) -->
    { throw(error('Missing => operator in pattern matching case', []) at Pos) }.

pattern_expr(Expr, _, Operators) -->
    expr(Expr, Operators),
    !.
pattern_expr(_, Pos, _) -->
    { throw(error('Invalid expression in pattern matching', []) at Pos) }.

guarded_pattern(Pattern at Start, Start) -->
    pattern(Pattern),
    !.
guarded_pattern(_, Start) -->
    { throw(error('Invalid pattern in pattern matching', []) at Start) }.

pattern(Pattern) -->
    atomic_pattern(Pattern),
    !.
pattern(Pattern) -->
    deconstructor_pattern(Pattern),
    !.

atomic_pattern(id(Id)) -->
    [id(Id) at _],
    !.
atomic_pattern(Pattern) -->
    ['[' at _],
    !,
    list_pattern(Pattern).
atomic_pattern(Pattern) -->
    ['(' at _],
    !,
    tuple_pattern(Pattern),
    [')' at _].
atomic_pattern(Const) -->
    pattern_constant(Const),
    !.

deconstructor_pattern(Deconstructor) -->
    [tid(Constructor) at _],
    deconstructor_pattern_args(Constructor, Deconstructor).

deconstructor_pattern_args(Constructor, deconstructor(Constructor, Arg)) -->
    atomic_pattern(Arg),
    !.
deconstructor_pattern_args(Constructor, deconstructor(Constructor)) --> [].

tuple_pattern(tuple([Pattern | Tail])) -->
    pattern(Pattern),
    tuple_pattern_tail(Tail).

tuple_pattern_tail([Pattern | Tail]) -->
    [',' at _],
    !,
    pattern(Pattern),
    tuple_pattern_tail(Tail).
tuple_pattern_tail([]) -->
    [].

list_pattern([]) -->
    [']' at _],
    !.
list_pattern([Elements | Tail]) -->
    tuple_pattern(tuple(Elements)),
    list_pattern_tail(Tail),
    [']' at _].

list_pattern_tail(id(Tail)) -->
    [operator('|') at _],
    !,
    [id(Tail) at _].
list_pattern_tail([]) --> [].

pattern_constant(integer(C)) -->
    [integer(C) at _],
    !.
pattern_constant(float(C)) -->
    [float(C) at _],
    !.
pattern_constant(string(C)) -->
    [string(C) at _],
    !.
pattern_constant(char(C)) -->
    [char(C) at _],
    !.
pattern_constant(unit) -->
    unit_literal(_).

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

empty_operator_list(D) :-
    dict_create(D, ops, []).

update_operators(Op, Priority, Assoc, Ops, NewOps) :-
    put_dict(Op, Ops, (Priority, Assoc), NewOps).

get_operators(OpsDict, Priority, Assoc, MatchingOperators) :-
    dict_pairs(OpsDict, _, Ops),
    filter_operators(Ops, Priority, Assoc, MatchingOperators).

filter_operators([], _, _, []) :-
    !.
filter_operators([Op-(Priority, Assoc) | Ops], Priority, Assoc, [Op | OpT]) :-
    !,
    filter_operators(Ops, Priority, Assoc, OpT).
filter_operators([_ | Ops], Priority, Assoc, Filtered) :-
    filter_operators(Ops, Priority, Assoc, Filtered).