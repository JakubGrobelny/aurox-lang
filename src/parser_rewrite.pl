:- [lexer].

parser_file(FileName, AST) :-
    tokenize_file(FileName, Tokens),
    empty_operator_list(Operators),
    catch(
        phrase(program(AST, Ops), Tokens),
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
    define_statement(DefineStart, Definition),
    program(Program, Operators).
program([FunctionDefinition | Program], Operators) -->
    [keyword(defun) at DefunStart],
    !,
    defun_statement(DefunStart, FunctionDefinition),
    program(Program, Operators).
program([TypeDefinition | Program], Operators) -->
    [keyword(type) at TypeDefStart],
    !,
    type_definition(TypeDefStart, TypeDefinition),
    program(Program, Operators).
program([Expression | Program], Operators) -->
    peek(_ at ExprStart),
    !
    expression(ExprStart, Expression).
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
    !.
    list_of_files_to_import(Start, Files),
list_of_files_to_import(_, []) --> [].

file_name_to_import(_, module_name(Module) at Pos) -->
    [tid(Module) at Pos],
    !.
file_name_to_import(_, file_name(File) at Pos) -->
    [string(File) at Pos],
    !.
file_name_to_import(Start, _) -->
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

define_statement(Start, define(Name, Arguments, Type, Value) at Start) -->
    valid_variable_name(Name),
    !,
    formal_parameters(Arguments),
    expected_token(Start, ':', colon, _),
    type(Type),
    expected_token(Start, operator('='), 'assignment operator', _),
    expression(Start, Value),
    expected_token(Start, operator(end), 'end keyword', _).
define_statement(Start, _) -->
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
%              TYPES                %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
type_definition_params(Start, _) -->
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
    type_definition_case(CaseStart, Constructor).
type_definition_cases(Start, _) -->
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
type_definition_constructor_type(Start, _) -->
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

operator_definition(Start, OldOperators, NewOperators) -->
    [operator(Op) at _],
    [integer(Priority) at PStart],
    [id(Associativity) at AStart},
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
        ).
    }.
operator_definition(Start, _, _) -->
    { throw(error('Syntax error in operator definition', []) at Start) }.

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
    put_dict(Op, UnaryAssoc), Ops, (Priority, UnaryAssoc), NewOps).

is_operator_type(Op, Priority, Assoc, Operators) :-
    member(Assoc, [left, right, none]),
    !,    
    get_dict((Op, infix), Operators, (Priority, Assoc)).
is_operator_type(Op, Priority, UnaryAssoc, Operators) :-
    get_dict((Op, UnaryAssoc), Operators, (Priority, Assoc)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%             AUXILIARY             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expected_token(_, TokenVal, _, Pos) -->
    [TokenVal at Pos],
    !.
expected_token(Start, _, TokenName _) -->
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

peek(Token), [Token] -->
    [Token].
