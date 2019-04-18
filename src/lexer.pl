char_codes_to_atoms([], []) :-
    !.
char_codes_to_atoms([Char | Chars], [Atom | Atoms]) :-
    char_code(Atom, Char),
    char_codes_to_atoms(Chars, Atoms).

tokenize_file(FileName, Tokens, pos(L, C)) :-
    open(FileName, read, Stream),
    read_string(Stream, "", "", _, String),
    string_to_list(String, ListOfChars),
    char_codes_to_atoms(ListOfChars, ListOfAtoms),
    phrase(lexer(Tokens, pos(1,1), pos(L, C)), ListOfAtoms),
    close(Stream).

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

comment      --> ['#'], comment_tail.
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

operator(op(Op), N) --> special(OpHead), operator_tail(OpTail, M), {
    N is M + 1,
    atomic_list_concat([OpHead | OpTail], Op)
}.
operator_tail([OpHead | OpTail], N) --> 
    special(OpHead), !, operator_tail(OpTail, M), { N is M + 1 }.
operator_tail([], 1) --> [].


lexer(Tokens, pos(L, C), Pos) -->
    whitespace, !, { NC is C + 1 }, lexer(Tokens, pos(L, NC), Pos).
lexer(Tokens, pos(L, _), Pos) --> 
    newline, !, { NL is L + 1 }, lexer(Tokens, pos(NL, 1), Pos).
lexer([], PosAcc, PosAcc) --> [].

:- op(1200, xfx, user:(==>)).
% TODO: expand_term/2












