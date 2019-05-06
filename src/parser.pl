:- ensure_loaded(lexer).
:- ensure_loaded(imports).

parse_file(FileName, AST) :-
    parse_file(FileName, AST, pos(FileName,0,0), [], _, _).

parse_file(FileName, AST, Start, Imported, NewDeps, NewOperators) :-
    tokenize_file(FileName, Tokens, Start),
    empty_operator_list(Operators),
    catch(
        phrase(
            program(
                AST, 
                Operators, 
                NewOperators, 
                [file_name(FileName) | Imported],
                NewDeps
            ), 
            Tokens
        ),
        error(Format, Args) at Pos,
        print_error_and_halt(Pos, Format, Args)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%         MAIN PARSER RULES         %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

program(Program, OldOperators, FinalOperators, Deps, NewDeps) -->
    [keyword(defop) at DefOpStart],
    !,
    operator_definition(DefOpStart, OldOperators, NewOperators),
    program(Program, NewOperators, FinalOperators, Deps, NewDeps).
program(DepsAST, Operators, NewOperators, Deps, NewDeps) -->
    [keyword(import) at ImportStart],
    !,
    import_statement(ImportStart, Files),
    { 
        import_dependencies(
            Files, % contents of the import statement
            Deps,   % already imported modules
            DepsOperators, % operators defined in dependencies
            UpdatedDeps, % list of imported modules after this import
            DepsAST, % AST of imported dependencies with Program as tail
            Program % uninstantiated variable used to append rest of the program
        ),
        merge_operators(Operators, DepsOperators, MergedOperators)
    },
    program(Program, MergedOperators, NewOperators, UpdatedDeps, NewDeps).
program([Definition | Program], Operators, NewOperators, Deps, NewDeps) -->
    [keyword(define) at DefineStart],
    !,
    define_statement(DefineStart, Definition, Operators),
    program(Program, Operators, NewOperators, Deps, NewDeps).
program([TypeDefinition | Program], Operators, NewOperators, Deps, NewDeps) -->
    [keyword(type) at TypeDefStart],
    !,
    type_definition(TypeDefStart, TypeDefinition),
    program(Program, Operators, NewOperators, Deps, NewDeps).
program([Expression | Program], Operators, NewOperators, Deps, NewDeps) -->
    peek(_ at ExprStart),
    !,
    expression_top_level(ExprStart, Expression, Operators),
    program(Program, Operators, NewOperators, Deps, NewDeps).
program([], Operators, Operators, Deps, Deps) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%          FILE IMPORTS             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import_statement(Start, Files) -->
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

define_statement(Start, Definition at Start, Ops) -->
    valid_function_name(Name),
    !,
    formal_parameters(Arguments),
    expected_token(Start, ':', colon, _),
    type(Type),
    expected_token(Start, op('='), 'assignment operator', _),
    peek(_ at ExprStart),
    expression_top_level(ExprStart, Value, Ops),
    expected_token(Start, keyword(end), 'end keyword', _),
    { 
        desugar_function_definition(
            define(Name, Arguments, Type, Value), Definition
        ) 
    }.
define_statement(Start, _, _) -->
    { throw(error('Syntax error in definition', []) at Start) }.

valid_function_name(Name) -->
    valid_identifier(Name),
    !.
valid_function_name(op(Name)) -->
    ['(' at _],
    [op(Name) at _],
    [')' at _].

valid_variable_name(wildcard) -->
    [keyword('_') at _],
    !.
valid_variable_name(Id) -->
    valid_identifier(Id).

valid_identifier(id(Name)) -->
    [id(Name) at _].

formal_parameters([Param | Params]) -->
    valid_variable_name(id(Param)),
    !,
    formal_parameters(Params).
formal_parameters([]) --> [].

desugar_function_definition(
    define(op(Op), [Arg], Type, Value at Pos),
    define(id(UnOp), Type, lambda([Arg], Value) at Pos)
) :-
    !,
    mark_unary_operator(Op, UnOp).
desugar_function_definition(define(op(Op), Args, Type, Val), Desugared) :-
    !,
    desugar_function_definition(define(id(Op), Args, Type, Val), Desugared).
desugar_function_definition(
    define(Name, [], Type, Value),
    define(Name, Type, Value)
) :- !.
desugar_function_definition(define(Name, Args, _, _ at Pos), _) :-
    not_unique_list(Args, Arg),
    !,
    Name =.. [_, N],
    print_error_and_halt(
        Pos,
        'definition of function ~w contains non unique parameter ~w',
        [N, Arg]
    ).
desugar_function_definition(
    define(Name, Args, Type, Value at Pos),
    define(Name, Type, lambda(Args, Value) at Pos)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%        TYPE DEFINITIONS           %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_definition(Start, typedef(Name, Params, Constructors) at Start) -->
    type_name(Start, Name),
    type_definition_params(Start, Params),
    expected_token(Start, keyword(with), 'with keyword', _),
    type_definition_cases(Start, Constructors),
    expected_token(Start, keyword(end), 'end keyword', _).

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
    peek(Token),
    { 
        \+ Token = keyword(with) at _,
        !,
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
    peek(Token),
    {
        \+ Token = keyword(end) at _,
        !,
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
    peek(Token),
    {
        \+ Token = keyword(case) at _,
        \+ Token = keyword(end) at _,
        !,
        throw_invalid_token(
            'Constructor can only be followed by atomic type',
            'constructor definition',
            Token
        )
    }.
type_definition_constructor_type(_, none) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%        OPERATOR DEFINITIONS       %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

operator_definition(_, OldOperators, NewOperators) -->
    [op(Op) at _],
    [int(Priority) at PStart],
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

operator(Op, Priority, Assoc, Pos, Ops, NOps) -->
    [op(Op) at Pos],
    { 
        \+ member(Op, ['|']),
        define_operator_if_needed(Op, Priority, Assoc, Ops, NOps) 
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%              TYPES                %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type(Type) -->
    tuple_type(Type),
    !.
type(_) --> [].

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
    [op('->') at _],
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
atomic_type(adt(Name, [])) -->
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

expression_sequence(Result, Operators) -->
    expression(Expr, Operators),
    !,
    expression_sequence_tail(Exprs, Operators),
    { Exprs = [] -> Result = Expr; Result = [Expr | Exprs] }.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%       ARITHMETIC EXPRESSIONS      %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tuple_expression(Expr, Operators) -->
    expr_logical_or(Lhs, Operators),
    tuple_expression_tail(Rhs, Operators),
    { construct_tuple([Lhs | Rhs], Expr) }.
tuple_expression_tail([Expr | Exprs], Operators) -->
    [',' at _],
    !,
    expr_logical_or(Expr, Operators),
    tuple_expression_tail(Exprs, Operators).
tuple_expression_tail([], _) --> [].

expr_logical_or(Expr, Operators) -->
    expr_logical_and(Lhs, Operators),
    expr_logical_or_rest(Lhs, Expr, Operators).
expr_logical_or_rest(Lhs, or(Lhs, Rhs), Operators) -->
    [keyword(or) at _],
    !,
    expr_logical_and(Rhs, Operators).
expr_logical_or_rest(Lhs, Lhs, _) --> [].

expr_logical_and(Expr, Operators) -->
    expr_n(0, Lhs, Operators),
    expr_logical_and_rest(Lhs, Expr, Operators).
expr_logical_and_rest(Lhs, and(Lhs, Rhs), Operators) -->
    [keyword(and) at _],
    !,
    expr_n(0, Rhs, Operators).
expr_logical_and_rest(Lhs, Lhs, _) --> [].

expr_n(Priority, Expr, Operators) -->
    expr_r(Priority, Lhs, Operators),
    expr_n_rest(Priority, Rhs, Operators),
    { merge_expr(Lhs, Rhs, Expr) }.
expr_n_rest(Priority, [Op, Rhs], Operators) -->
    operator(Op, Priority, none, _, Operators, NOperators),
    !,
    expr_r(Priority, Rhs, NOperators).
expr_n_rest(_, [], _) --> [].

expr_r(Priority, Expr, Operators) -->
    expr_l(Priority, Lhs, Operators),
    expr_r_rest(Priority, Rhs, Operators),
    { merge_expr(Lhs, Rhs, Expr) }.
expr_r_rest(Priority, [Op, Expr], Operators) -->
    operator(Op, Priority, right, _, Operators, NOperators),
    !,
    expr_l(Priority, Rhs, NOperators),
    expr_r_rest(Priority, ExprRest, NOperators),
    { merge_expr(Rhs, ExprRest, Expr) }.
expr_r_rest(_, [], _) --> [].

expr_l(Priority, Expr, Operators) -->
    expr_u_r(Priority, Lhs, Operators),
    expr_l_rest(Priority, Lhs, Expr, Operators).
expr_l_rest(Priority, Acc, Result, Operators) -->
    operator(Op, Priority, left, _, Operators, NOperators),
    !,
    expr_u_r(Priority, Rhs, NOperators),
    expr_l_rest(Priority, app(app(id(Op), Acc), Rhs), Result, NOperators).
expr_l_rest(_, Acc, Acc, _) --> [].

expr_u_r(Priority, Expr, Operators) -->
    expr_u_l(Priority, Arg, Operators),
    expr_u_r_rest(Priority, Arg, Expr, Operators).
expr_u_r_rest(Priority, Arg, app(id(UnOp), Arg), Operators) -->
    operator(Op, Priority, right_unary, _, Operators, _),
    !,
    { mark_unary_operator(Op, UnOp) }.
expr_u_r_rest(_, Arg, Arg, _) --> [].

expr_u_l(20, app(id(UnOp), Arg), Operators) -->
    operator(Op, 20, left_unary, _, Operators, NOperators),
    !,
    { mark_unary_operator(Op, UnOp) },
    application(Arg, NOperators).
expr_u_l(20, Expr, Operators) -->
    !,
    application(Expr, Operators).
expr_u_l(Priority, app(id(UnOp), Arg), Operators) -->
    operator(Op, Priority, left_unary, _, Operators, NOperators),
    !,
    { 
        N is Priority + 1,
        mark_unary_operator(Op, UnOp)
    },
    expr_n(N, Arg, NOperators).
expr_u_l(Priority, Expr, Operators) -->
    { N is Priority + 1 },
    expr_n(N, Expr, Operators).

application(Expr, Operators) -->
    atomic_expression(Lhs, Operators),
    application_rest(Lhs, Expr, Operators).
application_rest(Acc, Result, Operators) -->
    atomic_expression(Arg, Operators),
    !,
    application_rest(app(Acc, Arg), Result, Operators).
application_rest(Acc, Acc, _) --> [].

constant(int(N)) -->
    [int(N) at _],
    !.
constant(bool(B)) -->
    [bool(B) at _],
    !.
constant(unit) -->
    ['(' at _],
    !,
    [')' at _].
constant(float(X)) -->
    [float(X) at _],
    !.
constant(list(StrChars)) -->
    [string(Str) at _],
    !,
    { map_char(Str, StrChars) }.
constant(char(C)) -->
    [char(C) at _].

map_char([], []) :- !.
map_char([C | Cs], [char(C) | Chars]) :-
    map_char(Cs, Chars).

merge_expr(Lhs, [Op, Rhs], app(app(id(Op), Lhs), Rhs)) :-
    !.
merge_expr(Lhs, [], Lhs).

lambda(lambda(Params, Expr), Operators) -->
    ['{' at Start],
    !,
    lambda_param_list(Start, Params),
    { unique_lambda_parameters(Params, Start) },
    expression_top_level(Start, Expr at _, Operators),
    expected_token(Start, '}', 'closing curly bracket', _).
lambda_param_list(Start, Params) -->
    expected_token(Start, op('|'), 'lambda parameter list delimiter', _),
    formal_parameters(Params),
    expected_token(Start, op('|'), 'lambda parameter list delimiter', _).

list_expression(_, list([]), _) -->
    [']' at _],
    !.
list_expression(Start, list([H | T]), Operators) -->
    list_element(Start, H, Operators),
    !,
    list_expression_tail(Start, list(T), Operators).
list_expression_tail(Start, list(Tail), Operators) -->
    [op('|') at _],
    !,
    list_element(Start, Tail, Operators),
    expected_token(Start, ']', 'closing square bracket', _).
list_expression_tail(Start, list([H | T]), Operators) -->
    [',' at _],
    !,
    list_element(Start, H, Operators),
    list_expression_tail(Start, list(T), Operators).
list_expression_tail(Start, list([]), _) -->
    expected_token(Start, ']', 'closing square bracket', _).

list_element(_, Elem, Operators) -->
    expr_n(0, Elem, Operators),
    !.
list_element(Start, _) -->
    { throw(error('Syntax error in list literal', []) at Start) }.

atomic_expression(Lambda, Operators) -->
    lambda(Lambda, Operators),
    !.
atomic_expression(Const, _) -->
    constant(Const),
    !.
atomic_expression(id(Constructor),_) -->
    [tid(Constructor) at _],
    !.
atomic_expression(Var, _) -->
    valid_identifier(Var),
    !.
atomic_expression(Expr, Operators) -->
    ['(' at Start],
    !,
    expression_top_level(Start, Expr at _, Operators),
    expected_token(Start, ')', 'closing parenthesis', _).
atomic_expression(List, Operators) -->
    ['[' at Start],
    !,
    list_expression(Start, List, Operators).

unique_lambda_parameters(Params, Pos) :-
    not_unique_list(Params, Param),
    !,
    print_error_and_halt(
        Pos,
        'function literal contains a non unique parameter ~w',
        [Param]
    ).
unique_lambda_parameters(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%     CONDITIONAL EXPRESSIONS       %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conditional_expression(Start, if(Cond, Cons, Alt), Operators) -->
    expression_top_level(Start, Cond, Operators),
    expected_token(Start, keyword(then), 'then keyword', ThenStart),
    expression_top_level(ThenStart, Cons, Operators),
    conditional_expression_else(Start, Alt, Operators).

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
    expected_token(Start, op('='), 'assignment operator', _),
    expression_top_level(Start, Value, Operators),
    expected_token(Start, keyword(in), 'in keyword', _),
    peek(_ at ExprStart),
    expression_top_level(ExprStart, Expression, Operators),
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
    expected_token(Start, keyword(with), 'with keyword', _),
    pattern_matching_cases(Start, Patterns, Operators),
    expected_token(Start, keyword(end), 'end keyword', _).

pattern_matching_cases(Start, [Case | Cases], Operators) -->
    pattern_matching_case(Start, Case, Operators),
    !,
    pattern_matching_cases(Start, Cases, Operators).
pattern_matching_cases(_, [], _) --> [].

pattern_matching_case(_, '=>'(Pattern, Expr), Operators) -->
    [keyword(case) at CaseStart],
    pattern_guard(CaseStart, Pattern),
    expected_token(CaseStart, op('=>'), '=> operator', _),
    expression_top_level(CaseStart, Expr, Operators).

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
    [tid(Constructor) at _],
    !,
    deconstructor_pattern_argument(Constructor, Pattern).
deconstructor_pattern(Pattern) -->
    atomic_pattern(Pattern).
deconstructor_pattern_argument(Constructor, adt(Constructor, Argument)) -->
    atomic_pattern(Argument),
    !.
deconstructor_pattern_argument(Constructor, enum(Constructor)) --> [].

atomic_pattern(Pattern) -->
    valid_variable_name(Pattern),
    !.
atomic_pattern(enum(Enum)) -->
    [tid(Enum) at _],
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

list_pattern(list([])) -->
    [']' at _],
    !.
list_pattern(list([H | T])) -->
    deconstructor_pattern(H),
    list_pattern_tail(list(T)).
list_pattern_tail(list([H | T])) -->
    [',' at _],
    !,
    deconstructor_pattern(H),
    list_pattern_tail(list(T)).
list_pattern_tail(list([])) -->
    [']' at _],
    !.
list_pattern_tail(list(Tail)) -->
    [op('|') at _],
    valid_variable_name(Tail),
    [']' at _].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%             AUXILIARY             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

construct_tuple([], none) :- !.
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
            'Invalid token "~w" of type "~w" in ~w. ~w',
            [Value, Functor, Context, Expected]
        ) at Pos
    ).
throw_invalid_token(Expected, Context, Token at Pos) :-
    throw(
        error(
            'Invalid token "~w" in ~w. ~w',
            [Token, Context, Expected]
        ) at Pos
    ).

peek(Token), [Token] --> [Token].
