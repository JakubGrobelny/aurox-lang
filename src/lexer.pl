:- [utility].
:- op(200, xfx, user:(at)).

char_codes_to_atoms([], [eof]) :-
    !.
char_codes_to_atoms([Char | Chars], [Atom | Atoms]) :-
    char_code(Atom, Char),
    char_codes_to_atoms(Chars, Atoms).

handle_failure([], _) :-
    !.
handle_failure([error(Error) at Pos | _], FileName) :-
    print_error(Pos, FileName, 'invalid token "~w"', [Error]),
    halt.

tokenize_file(FileName, Tokens) :-
    open(FileName, read, Stream),
    read_string(Stream, "", "", _, String),
    close(Stream),
    string_to_list(String, ListOfChars),
    char_codes_to_atoms(ListOfChars, ListOfAtoms),
    phrase(lexer(Tokens, pos(1,1)), ListOfAtoms, Rest),
    handle_failure(Rest, FileName).

lowercase(Char) --> [Char], { char_type(Char, lower) }.

uppercase(Char) --> [Char], { char_type(Char, upper) }.

letter(Char) --> lowercase(Char).
letter(Char) --> uppercase(Char).

alphanum_char(Char) --> letter(Char).
alphanum_char(Char) --> digit(Char).

digit(D) --> [D], { char_type(D, digit) }.

whitespace --> [' '].
whitespace --> ['\r'].
whitespace --> ['\t'].

newline --> ['\n'].

comment_tail, [eof] --> [eof], !.
comment_tail --> ['\n'], !.
comment_tail --> [_], comment_tail.

alphanum([Char | Chars], Len) --> alphanum_char(Char), !, alphanum(Chars, L0), {
    Len is L0 + 1 
}.
alphanum([],  0) --> [].

type_name(tid(Id), Len) --> uppercase(Char), alphanum(Tail, TLen), {
    Len is TLen,
    atomic_list_concat([Char | Tail], Id)
}.

identifier(id(Id), Len) --> lowercase(Char), alphanum(Tail, TLen), {
    Len is 1 + TLen,
    atomic_list_concat([Char | Tail], Id)
}.

digit_seq([Digit | DTail], Len) --> digit(Digit), digit_seq_tail(DTail, Len).
digit_seq_tail([Digit | DTail], Len) --> 
    digit(Digit), !, digit_seq_tail(DTail, TLen), { Len is TLen }.
digit_seq_tail([], 0) --> [].

integer_literal(int(Int), Len) --> digit_seq(Atoms, Len), {
    atomic_list_concat(Atoms, IntAtom),
    atom_number(IntAtom, Int)
}.
% TODO: merge integer_literal with float_literal to avoid reading the initial
%       sequence twice

special(Char) --> [Char], {
    member(Char, ['-', '+', '*', '/', '=', ':', '>', '<', 
                  '!', '@', '%', '^', '~', '&', '$', '|']
    )
}.

continuous_sequence([]) --> [eof], !.
continuous_sequence([]) --> whitespace, !.
continuous_sequence([]) --> newline, !.
continuous_sequence([X | Xs]) --> [X], !, continuous_sequence(Xs).
continuous_sequence([]) --> [].

operator_tail([OpHead | OpTail], N) --> 
    special(OpHead), !, operator_tail(OpTail, M), { N is M + 1 }.
operator_tail([], 1) --> [].

lexer([], _) --> [eof], !.
lexer(Tokens, pos(L, C)) -->
    whitespace, !, { NC is C + 1 }, lexer(Tokens, pos(L, NC)).
lexer(Tokens, pos(L, _)) --> 
    newline, !, { NL is L + 1 }, lexer(Tokens, pos(NL, 1)).
lexer(Tokens, pos(L, _)) -->
    ['#'], !, comment_tail, { NL is L + 1 }, lexer(Tokens, pos(NL, 1)).
lexer([op(Op) at pos(L, C) | Tokens], pos(L, C)) --> 
    special(OpHead), !, operator_tail(OpTail, Len), 
    { NC is C + Len }, lexer(Tokens, pos(L, NC)), {
        atomic_list_concat([OpHead | OpTail], Op)
    }.
lexer([tid(TId) at pos(L, C) | Tokens], pos(L, C)) -->
    uppercase(Letter), !, alphanum(Tail, Len), { NC is C + Len + 1 },
    lexer(Tokens, pos(L, NC)), {
        atomic_list_concat([Letter | Tail], TId)
    }.
lexer([], Pos), [error(Err) at Pos] --> [X], continuous_sequence(Characters), {
    atomic_list_concat([X | Characters], Err)
}.

:- op(1200, xfx, user:(==>)).
% TODO: expand_term/2












