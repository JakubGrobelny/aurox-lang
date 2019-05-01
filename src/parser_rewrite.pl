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
    top_level_expression(ExprStart, Expression).
program([], _) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%          FILE IMPORTS             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import_statement(Start, import(Files)) -->
    curly_bracket(BracketStart, Start),
    list_of_files_to_import(Start, Files),
    curly_bracket_terminator(BracketStart).

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
    [Token at Pos],
    {
        \+ Token = '}',
        throw_invalid_file_name_token(Token, Pos)
    },
    !.

throw_invalid_file_name_token(Token, Pos) :-
    Token =.. [Functor, Value],
    !,
    throw(
        error(
            'Invalid token ~w of type ~w in import statement.\c 
             Valid import specifiers are either capitalized module names \c
             or strings with file names', 
            [Functor, Value]
        ) at Pos
    ).
throw_invalid_file_name_token(Token, Pos) :-
    throw(
        error(
            'Unexpected token ~w in import statement.'
             [Token]
        ) at Pos
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%              DEFINE               %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

define_statement(Start, define(Name, Arguments, Type, Value) at Start) -->
    valid_variable_name(Start, Name),
    formal_parameters(Start, Arguments),
    colon(Start),
    type(Type),
    assignment_operator(Start),
    top_level_expression(Start, Value).

valid_variable_name(_, id(Name)) -->
    [id(Name) at _],
    !.
valid_variable_name(_, operator(Op)) -->
    ['(' at _],
    [operator(Op) at _],
    [')' at _],
    !.
valid_variable_name(DefinitionStart, _) -->
    [Something at Somewhere],
    !,


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%             AUXILIARY             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peek(Token), [Token] -->
    [Token].

throw_unexpected_token(Token, Position) :-
    Token =.. [Type, Value],
    throw(
        error(
            'Unexpected token ~w of type ~w', 
            [Value, Type]
        ) at Position
    ).

dot_terminator(_) -->
    ['.' at _],
    !.
dot_terminator(StatementStart, Where) -->
    {
        throw(
            error(
                'Missing expected dot terminator after ~w',
                [Where]
            ) at StatementStart
        )
    }.

curly_bracket(Position, _) -->
    ['{' at Position],
    !.
curly_bracket(_, Start) -->
    { throw(error('Missing opening curly bracket', []) at Start) }.

curly_bracket_terminator(_) -->
    ['}' at _],
    !.
curly_bracket_terminator(Start) -->
    { throw(error('Missing expected closing curly bracket', []) at Start) }.

square_bracket(Position, _) -->
    ['[' at Position],
    !.
square_bracket(_, Start) -->
    { throw(error('Missing opening square bracket', []) at Start) }.

square_bracket_terminator(_) -->
    [']' at _],
    !.
square_bracket_terminator(Start) -->
    { throw(error('Missing expected closing square bracket', []) at Start) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%        OPERATOR DEFINITIONS       %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

operator_definition(_, OldOperators, NewOperators) -->
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
    atomic_list_concat(
        ['Invalid operator associativity type ~w in operator declaration.',
         'Allowed types are left, right, none, left_unary and right_unary'], 
        ErrorMessage
    ),
    throw(error(ErrorMessage, [Assoc]) at Pos).

empty_operator_list(D) :-
    dict_create(D, ops, []).

update_operators(Op, Priority, Assoc, Ops, NewOps) :-
    put_dict(Op, Ops, (Priority, Assoc), NewOps).

is_operator_type(Op, Priority, Assoc, Operators) :-
    get_dict(Op, Operators, (Priority, Assoc)).


