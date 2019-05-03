:- [parser].

repl :-
    repl([]).

repl(PrevTokens) :-
    parse_stdin(PrevTokens, AST, Leftovers),
    eval(AST),
    repl(Leftovers).

% TODO: add parsing
parse_stdin(Prev, AST, Leftovers) :-
    read_line_to_codes(user_input, Line),
    char_codes_to_atoms(Line, Atoms),
    append(Prev, Atoms, Total),
    empty_operator_list(Operators),
    catch(
        (phrase(lexer(Tokens, pos(stdin, 1,1)), Total, Leftovers),
         phrase(program(AST, Operators), Tokens)),
        error(Format, Args) at Pos,
        print_error(Pos, Format, Args)
    ).

% TODO: implement
eval(Xs) :- 
    writeln(Xs).
