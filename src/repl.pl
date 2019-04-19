:- [parser].

repl :-
    repl([]).

repl(PrevTokens) :-
    parse_stdin(PrevTokens, AST, Leftovers),
    eval(AST),
    repl(Leftovers).

% TODO: add parsing
parse_stdin(Prev, Tokens, Leftovers) :-
    read_line_to_codes(user_input, Line),
    char_codes_to_atoms(Line, Atoms),
    append(Prev, Atoms, Total),
    append_eof(Total, AtomsEof),
    phrase(lexer(Tokens, pos(1,1)), AtomsEof, Leftovers),
    handle_failure(Leftovers, stdin).

% TODO: implement
eval(Xs) :- 
    write(Xs).

append_eof([], [' ', eof]) :-
    !.
append_eof([eof], [' ', eof]) :-
    !.
append_eof([X | Xs], [X | Ys]) :-
    append_eof(Xs, Ys).
