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

program(Program, Operators) -->
    [keyword(defop) at DefOpStart],
    !,
    operator_definition(DefOpStart, OldOperators, NewOperators),
    program(Program, NewOperators).
program([Definition | Program], Operators) -->
    definition(Definition, Operators),
    !,
    program(Program, Operators).
program([Expression], Operators) -->
    expression(Expression, Operators),
    !.
program(_, _) -->
    [Token at Position],
    !,
    { 
        throw(
            error(
                'Invalid expression or statement starting with token ~w', 
                [Token]
            ) at Position
        ) 
    }.
program([], _) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%           DEFINITIONS             %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

definition(Definition, Operators) -->
    [keyword(define) at Start],
    !,
    define(Definition, Operators, Start).
definition(Definition, Operators) -->
    [keyword(defun) at Start],
    !,
    defun(Definition, Operators, Start).
definition(TypeDef, Operators) -->
    [keyword(type) at Start],
    !,
    typedef(TypeDef, Operators, Start).
definition(Import, _) -->
    [keyword(import) at Start],
    import(Import, Start).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%              DEFINE               %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%         FILE IMPORTS              %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
files_to_import(_) -->
    [Something at Somewhere],
    !,
    { throw_unexpected_token(Something, Somewhere) }.
files_to_import([]) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   %
%      AUXILIARY                    %
%                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

throw_unexpected_token(Token, Position) :-
    Token =.. [Type, Value],
    throw(
        error(
            'Unexpected token ~w of type ~w', 
            [Value, Type]
        ) at Position
    ).

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
    ['{' at _],
    [operator(Operator) at _],
    [integer(Priority) at PriorityPos],
    [id(Associativity) at AssocPos],
    ['}' at _],
    {
        valid_priority(Priority, PriorityPos),
        valid_associativity(Associativity, AssocPos),
        update_operators(
            Operator,
            Priority,
            Associativity,
            OldOperators,
            NewOperators
        )
    },
    !.
operator_definition(DefOpStart, _, _) -->
    { throw(error('Syntax error in operator definition', []) at DefOpStart) }.

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







