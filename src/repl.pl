:- [parser].

% TODO: fix

% repl :-
%     empty_operator_list(Ops),
%     repl([], Ops).

% repl(PrevTokens, Operators) :-
%     parse_stdin(PrevTokens, AST, Leftovers, Operators, NewOperators),
%     eval(AST),
%     repl(Leftovers, NewOperators).

% parse_stdin(Prev, AST, Leftovers, Operators, NewOperators) :-
%     read_line_to_codes(user_input, Line),
%     char_codes_to_atoms(Line, Atoms),
%     append(Prev, Atoms, Total),
%     catch(
%         (phrase(lexer(Tokens, pos(stdin, 1,1)), Total, Leftovers),
%          phrase(program(AST, Operators, NewOperators), Tokens)),
%         error(Format, Args) at Pos,
%         print_error_and_halt(Pos, Format, Args)
%     ).

% eval(Xs) :- 
%     writeln(Xs).