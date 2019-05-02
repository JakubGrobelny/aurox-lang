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
    [Token],
    {
        \+ Token = '}',
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
    colon(Start),
    type(Type),
    assignment_operator(Start),
    top_level_expression(Start, Value).
define_statement(Start, _) -->
    { throw(error('Syntax error in definition', []) at Start) }.

valid_variable_name(id(Name)) -->
    [id(Name) at _],
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
%             AUXILIARY             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assignment_operator(_) -->
    [operator('=') at _],
    !.
assignment_operator(Start) -->
    { throw(error('Missing expected assignment operator', []) at Start) }.

colon(_) -->
    [':' at _],
    !.
colon(Start) -->
    { throw(error('Missing expected colon', []) at Start) }.

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
    },
    dot_terminator(Start).
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


