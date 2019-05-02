:- [lexer].

parse_file(FileName, AST) :-
    tokenize_file(FileName, Tokens),
    empty_operator_list(Operators),
    catch(
        phrase(program(AST, Operators), Tokens),
        error(Format, Args) at Pos,
        print_error(Pos, Format, Args)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%         MAIN PARSER RULES         %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(Program, OldOperators) -->
    [keyword(defop) at DefOpStart],
    !,
    operator_definition(DefOpStart, OldOperators, NewOperators),
    program(Program, NewOperators).
program([Import | Program], Operators) -->
    [keyword(import) at ImportStart],
    !,
    import_statement(ImportStart, Import),
    program(Program, Operators).
program([Definition | Program], Operators) -->
    [keyword(define) at DefineStart],
    !,
    define_statement(DefineStart, Definition, Operators),
    program(Program, Operators).
program([TypeDefinition | Program], Operators) -->
    [keyword(type) at TypeDefStart],
    !,
    type_definition(TypeDefStart, TypeDefinition),
    program(Program, Operators).
program([Expression | Program], Operators) -->
    peek(_ at ExprStart),
    !,
    expression_top_level(ExprStart, Expression, Operators),
    program(Program, Operators).
program([], _) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%          FILE IMPORTS             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import_statement(Start, import(Files)) -->
    list_of_files_to_import(Start, Files),
    expected_token(Start, keyword(end), 'end keyword', _).

list_of_files_to_import(Start, [File | Files]) -->
    file_name_to_import(Start, File),
    !,
    list_of_files_to_import(Start, Files).
list_of_files_to_import(_, []) --> [].

file_name_to_import(_, module_name(Module) at Pos) -->
    [tid(Module) at Pos],
    !.
file_name_to_import(_, file_name(File) at Pos) -->
    [string(File) at Pos],
    !.
file_name_to_import(_, _) -->
    [Token],
    {
        \+ Token = keyword(end) at _,
        throw_invalid_token(
            'Valid import specifiers are either capitalized module names \c
             or strings with file names',
             'import statement',
             Token)
    },
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%              DEFINE               %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

define_statement(Start, define(Name, Arguments, Type, Value) at Start, Ops) -->
    valid_variable_name(Name),
    !,
    formal_parameters(Arguments),
    expected_token(Start, ':', colon, _),
    type(Type),
    expected_token(Start, operator('='), 'assignment operator', _),
    expression_top_level(Start, Value, Ops),
    expected_token(Start, operator(end), 'end keyword', _).
define_statement(Start, _, _) -->
    { throw(error('Syntax error in definition', []) at Start) }.

valid_variable_name(id(Name)) -->
    [id(Name) at _],
    !.
valid_variable_name(wildcard) -->
    [keyword('_') at _],
    !.
valid_variable_name(operator(Op)) -->
    ['(' at _],
    [operator(Op) at _],
    [')' at _].

formal_parameters([Param | Params]) -->
    valid_variable_name(Param),
    !,
    formal_parameters(Params).
formal_parameters([]) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%        TYPE DEFINITIONS           %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_definition(Start, typedef(Name, Params, Constructors)) -->
    type_name(Start, Name),
    type_definition_params(Start, Params),
    expected_token(Start, with, 'with keyword', _),
    type_definition_cases(Start, Constructors),
    expected_token(Start, end, 'end keyword', _).

type_name(_, TypeName) -->
    [tid(TypeName) at _],
    !.
type_name(Start, _) -->
    { throw(error('Invalid type name', []) at Start) }.

type_definition_params(Start, [Param | Params]) -->
    [id(Param) at _],
    !,
    type_definition_params(Start, Params).
type_definition_params(_, _) -->
    [Token],
    !,
    { 
        \+ Token = keyword(with) at _,
        throw_invalid_token(
            'Type parameters are lowercase identifiers', 
            'type definition', 
            Token
        ) 
    }.
type_definition_params(_, []) --> [].

type_definition_cases(Start, [Constructor | Constructors]) -->
    [keyword(case) at CaseStart],    
    !,
    type_definition_case(CaseStart, Constructor),
    type_definition_cases(Start, Constructors).
type_definition_cases(_, _) -->
    [Token],
    !,
    {
        \+ Token = operator(end) at _,
        throw_invalid_token(
            'Type definition cases must start with case keyword',
            'type definition',
            Token
        )
    }.
type_definition_cases(_, []) --> [].

type_definition_case(CaseStart, constructor(Name, Type)) -->
    [tid(Name) at _],
    type_definition_constructor_type(CaseStart, Type).

type_definition_constructor_type(_, Type) -->
    atomic_type(Type),
    !.
type_definition_constructor_type(_, _) -->
    [Token],
    !,
    {
        \+ Token = operator(case) at _,
        throw_invalid_token(
            'Constructor can only be followed by atomic type',
            'constructor definition',
            Token
        )
    }.
type_definition_constructor_type(_, undefined) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%        OPERATOR DEFINITIONS       %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

operator_definition(_, OldOperators, NewOperators) -->
    [operator(Op) at _],
    [integer(Priority) at PStart],
    [id(Associativity) at AStart],
    !,
    {
        valid_priority(Priority, PStart),
        valid_associativity(Associativity, AStart),
        update_operators(
            Op, 
            Priority, 
            Associativity, 
            OldOperators, 
            NewOperators
        )
    }.
operator_definition(Start, _, _) -->
    { throw(error('Syntax error in operator definition', []) at Start) }.

operator(Priority, Assoc, Op, Pos, Ops) -->
    [operator(Op) at Pos],
    { is_operator_type(Op, Priority, Assoc, Ops) }.

valid_priority(N, _) :-
    member(N, [0,1,2,3,4,5]),
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

empty_operator_list(D) :-
    dict_create(D, ops, []).

infix(Assoc) :-
    member(Assoc, [left, right, none]).

update_operators(Op, Priority, Assoc, Ops, NewOps) :-
    infix(Assoc),
    !,
    put_dict((Op, infix), Ops, (Priority, Assoc), NewOps).
update_operators(Op, Priority, UnaryAssoc, Ops, NewOps) :-
    put_dict((Op, UnaryAssoc), Ops, (Priority, UnaryAssoc), NewOps).

is_operator_type(Op, Priority, Assoc, Operators) :-
    member(Assoc, [left, right, none]),
    !,    
    get_dict((Op, infix), Operators, (Priority, Assoc)).
is_operator_type(Op, Priority, UnaryAssoc, Operators) :-
    get_dict((Op, UnaryAssoc), Operators, (Priority, UnaryAssoc)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%              TYPES                %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type(Type) -->
    tuple_type(Type),
    !.
type(undefined) --> [].

tuple_type(Type) -->
    function_type(Left),
    tuple_type_tail(Rest),
    { construct_tuple([Left | Rest], Type) }.
tuple_type_tail([Elem | Tail]) -->
    [',' at _],
    !,
    function_type(Elem),
    tuple_type_tail(Tail).
tuple_type_tail([]) --> [].

function_type(Type) -->
    algebraic_type(Left),
    function_type_tail(Tail),
    { construct_with_functor('->', [Left | Tail], Type) }.
function_type_tail([Type | Types]) -->
    [operator('->') at _],
    !,
    algebraic_type(Type),
    function_type_tail(Types).
function_type_tail([]) --> [].

algebraic_type(adt(TypeName, Parameters)) -->
    [tid(TypeName) at _],
    !,
    atomic_type_sequence(Parameters).
algebraic_type(Atomic) -->
    atomic_type(Atomic).
atomic_type_sequence([Type | Types]) -->
    atomic_type(Type),
    !,
    atomic_type_sequence(Types).
atomic_type_sequence([]) --> [].

atomic_type(param(Id)) -->
    [id(Id) at _],
    !.
atomic_type(name(Name)) -->
    [tid(Name) at _],
    !.
atomic_type(list(Type)) -->
    ['[' at _],
    !,
    type(Type),
    [']' at _].
atomic_type(Type) -->
    ['(' at _],
    !,
    type(Type),
    [')' at _].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%           EXPRESSIONS             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expression_top_level(Start, Expr at Start, Operators) -->
    expression_sequence(Expr, Operators),
    !.
expression_top_level(Start, _, _) -->
    { throw(error('Syntax error in expression', []) at Start) }.

expression_sequence([Expr | Exprs], Operators) -->
    expression(Expr, Operators),
    !,
    expression_sequence_tail(Exprs, Operators).
expression_sequence_tail([Expr | Exprs], Operators) -->
    [';' at _],
    !,
    expression(Expr, Operators),
    expression_sequence_tail(Exprs, Operators).
expression_sequence_tail([], _) --> [].

expression(PMatch, Operators) -->
    [keyword(match) at Start],
    !,
    pattern_matching(Start, PMatch, Operators).
expression(LetDef, Operators) -->
    [keyword(let) at Start],
    !,
    let_definition(Start, LetDef, Operators).
expression(Conditional, Operators) -->
    [keyword(if) at Start],
    !,
    conditional_expression(Start, Conditional, Operators).
expression(Tuple, Operators) -->
    tuple_expression(Tuple, Operators).
% TODO: consider changing tuple priority to be
%       lower than other expression types in 
%       expression(...) rule

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%       ARITHMETIC EXPRESSIONS      %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tuple_expression(Expr, Operators) -->
    expr_n(0, Lhs, Operators),
    tuple_expression_tail(Rhs, Operators),
    { construct_tuple([Lhs | Rhs], Expr) }.
tuple_expression_tail([Expr | Exprs], Operators) -->
    [',' at _],
    !,
    expr_n(0, Expr, Operators),
    tuple_expression_tail(Exprs, Operators).
tuple_expression_tail([], _) --> [].

expr_n(Priority, Expr, Operators) -->
    expr_r(Priority, Lhs, Operators),
    expr_n_rest(Priority, Rhs, Operators),
    { merge_expr(Lhs, Rhs, Expr) }.
expr_n_rest(Priority, [Op, Rhs], Operators) -->
    operator(Op, Priority, none, _, Operators),
    !,
    expr_r(Priority, Rhs, Operators).
expr_n_rest(_, [], _) --> [].

expr_r(Priority, Expr, Operators) -->
    expr_l(Priority, Lhs, Operators),
    expr_l_rest(Priority, Rhs, Operators),
    { merge_expr(Lhs, Rhs, Expr) }.
expr_r_rest(Priority, [Op, Expr], Operators) -->
    operator(Op, Priority, right, _, Operators),
    expr_l(Priority, Rhs, Operators),
    expr_r_rest(Priority, ExprRest, Operators),
    { merge_expr(Rhs, ExprRest, Expr) }.

merge_expr(Lhs, [Op, Rhs], Expr) :-
    Expr =.. [Op, Lhs, Rhs],
    !.
merge_expr(Lhs, [], Lhs).


% TODO: 1. lists, 2. unit type

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%     CONDITIONAL EXPRESSIONS       %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conditional_expression(Start, if(Cond, Cons, Alt), Operators) -->
    expression_top_level(Start, Cond, Operators),
    expected_token(Start, keyword(then), 'then keyword', ThenStart),
    expression_top_level(ThenStart, Cons, Operators),
    conditional_expression_else(Start, Alt).

conditional_expression_else(_, unit, _) -->
    [keyword(end) at _],
    !.
conditional_expression_else(Start, Alt, Operators) -->
    expected_token(Start, keyword(else), 'else keyword', ElseStart),
    expression_top_level(ElseStart, Alt, Operators),
    expected_token(Start, keyword(end), 'end keyword', _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%         LET EXPRESSIONS           %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

let_definition(Start, let(Name, Type, Value, Expression), Operators) -->
    let_definition_name(Start, Name),
    expected_token(Start, ':', 'colon operator', _),
    type(Type),
    expected_token(Start, operator('='), 'assignment operator', _),
    expression_top_level(Start, Value, Operators),
    expected_token(Start, keyword(in), 'in keyword', _),
    expression_top_level(Start, Expression, Operators),
    expected_token(Start, keyword(end), 'end keyword', _).

let_definition_name(_, Name) -->
    valid_variable_name(Name),
    !.
let_definition_name(Start, _) -->
    { throw(error('Invalid variable name in let definition', []) at Start) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%         PATTERN MATCHING          %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pattern_matching(Start, pmatch(Expr, Patterns), Operators) -->
    expression_top_level(Start, Expr, Operators),
    expected_token(Start, keyword(with), 'with keyword', 'pattern matching'),
    pattern_matching_cases(Start, Patterns, Operators),
    expected_token(Start, keyword(end), 'end keyword', 'pattern matching').

pattern_matching_cases(Start, [Case | Cases], Operators) -->
    pattern_matching_case(Start, Case, Operators),
    !,
    pattern_matching_cases(Start, Cases, Operators).
pattern_matching_cases(_, [], _) --> [].

pattern_matching_case(Start, '=>'(Pattern, Expr), Operators) -->
    [keyword(case) at _],
    pattern_guard(Start, Pattern),
    expected_token(
        Start, 
        operator('=>'), 
        '=> operator', 
        'pattern matching case'
    ),
    expression_top_level(Start, Expr, Operators).

pattern_guard(_, Pattern) -->
    pattern(Pattern),
    !.
pattern_guard(Start, _) -->
    { throw(error('Syntax error in pattern matching pattern', []) at Start) }.

pattern(Pattern) -->
    deconstructor_pattern(Left),
    pattern_tail(Tail),
    { construct_tuple([Left | Tail], Pattern) }.
pattern_tail([P | Ps]) -->
    [',' at _],
    !,
    deconstructor_pattern(P),
    pattern_tail(Ps).
pattern_tail([]) --> [].

deconstructor_pattern(Pattern) -->
    atomic_pattern(Pattern),
    !.
deconstructor_pattern(Pattern) -->
    [tid(Constructor) at _],
    deconstructor_pattern_argument(Constructor, Pattern).
deconstructor_pattern_argument(Constructor, adt(Constructor, Argument)) -->
    atomic_pattern(Argument),
    !.
deconstructor_pattern_argument(Constructor, Constructor) --> [].

atomic_pattern(Pattern) -->
    valid_variable_name(Pattern),
    !.
atomic_pattern(Pattern) -->
    ['(' at _],
    !,
    pattern(Pattern),
    [')' at _].
atomic_pattern(Pattern) -->
    ['[' at _],
    !,
    list_pattern(Pattern).
atomic_pattern(Const) -->
    constant(Const).

list_pattern([]) -->
    [']' at _],
    !.
list_pattern([H | T]) -->
    deconstructor_pattern(H),
    list_pattern_tail(T).
list_pattern_tail([H | T]) -->
    [',' at _],
    !,
    deconstructor_pattern(H),
    list_pattern_tail(T).
list_pattern_tail([]) -->
    [']' at _],
    !.
list_pattern_tail([Tail]) -->
    [operator('|') at _],
    valid_variable_name(Tail).
    [']' at _].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%             AUXILIARY             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

construct_tuple([], undefined) :- !.
construct_tuple([X], X) :- !.
construct_tuple(Xs, tuple(N, Tuple)) :-
    length(Xs, N),
    construct_with_functor(',', Xs, Tuple).

construct_with_functor(_, [X], X) :- !.
construct_with_functor(F, [X | Xs], Result) :-
    construct_with_functor(F, Xs, PrevResult),
    Result =.. [F, X, PrevResult].

expected_token(_, TokenVal, _, Pos) -->
    [TokenVal at Pos],
    !.
expected_token(Start, _, TokenName, _) -->
    { throw(error('Missing expected ~w.', [TokenName]) at Start) }.

throw_invalid_token(Expected, Context, Token at Pos) :-
    Token =.. [Functor, Value],
    !,
    throw(
        error(
            'Invalid token ~w of type ~w in ~w. ~w',
            [Value, Functor, Context, Expected]
        ) at Pos
    ).
throw_invalid_token(Expected, Context, Token at Pos) :-
    throw(
        error(
            'Invalid token ~w in ~w. ~w',
            [Token, Context, Expected]
        ) at Pos
    ).

peek(Token), [Token] --> [Token].
