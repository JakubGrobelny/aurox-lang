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
    catch(
        (phrase(lexer(Tokens, pos(1,1)), Total, Leftovers),
         phrase(parser(AST), Tokens)),
        error(Format, Args) at Pos,
        print_error(Pos, stdin, Format, Args)
    ).

% TODO: implement
eval(Xs) :- 
    writeln(Xs).
