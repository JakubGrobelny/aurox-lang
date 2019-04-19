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
    append_eof(Total, AtomsEof),
    phrase(lexer(Tokens, pos(1,1)), AtomsEof, Leftovers),
    handle_lexing_failure(Leftovers, stdin),
    catch(
        phrase(parser(AST), Tokens),
        Error at Pos,
        handle_parsing_error(Error, stdin, Pos)
    ).


% TODO: implement
eval(Xs) :- 
    writeln(Xs).

append_eof([], [' ', eof]) :-
    !.
append_eof([eof], [' ', eof]) :-
    !.
append_eof([X | Xs], [X | Ys]) :-
    append_eof(Xs, Ys).
