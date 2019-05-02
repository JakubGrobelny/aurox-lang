:- [utility].
:- op(200, xfx, at).

char_codes_to_atoms([], []) :-
    !.
char_codes_to_atoms([Char | Chars], [Atom | Atoms]) :-
    char_code(Atom, Char),
    char_codes_to_atoms(Chars, Atoms).

tokenize_file(FileName, Tokens) :-
    open(FileName, read, Stream),
    read_string(Stream, "", "", _, String),
    close(Stream),
    string_to_list(String, ListOfChars),
    char_codes_to_atoms(ListOfChars, ListOfAtoms),
    catch(
        phrase(lexer(Tokens, pos(FileName, 1,1)), ListOfAtoms),
        error(Format, Args) at Pos,
        print_error(Pos, Format, Args)
    ).

lowercase(Char) --> [Char], { char_type(Char, lower) }.

uppercase(Char) --> [Char], { char_type(Char, upper) }.

letter(Char) --> lowercase(Char), !.
letter(Char) --> uppercase(Char).

alphanum_char('?') --> ['?'], !.
alphanum_char('_') --> ['_'], !.
alphanum_char(Char) --> letter(Char), !.
alphanum_char(Char) --> digit(Char).

digit(D) --> [D], { char_type(D, digit) }.

string_delimiter --> ['"'].

char_delimiter --> ['\''].

string_terminator(_, _) --> string_delimiter, !.
string_terminator(Str, Pos) -->
    { throw(error('Nonterminated string ~w', [Str]) at Pos) }.

char_literal(_, _, Start) -->
    ['\''],
    !,
    { throw(error('Empty character literal', []) at Start) }.
char_literal(Char, Len, _) -->
    char(Char, Len),
    char_delimiter,
    !.
char_literal(_, _, Start) -->
    char_sequence(Seq, Len),
    char_error(Seq, Start, Len).

char_error(Seq, Pos, _) -->
    char_delimiter,
    !,
    { 
        atomic_list_concat(Seq, Char),
        throw(error('Character literal ~w is too long', [Char]) at Pos) 
    }.
char_error(Char, Pos, _) -->
    { throw(error('Nonterminated character ~w', [Char]) at Pos) }.

whitespace --> [' '],  !.
whitespace --> ['\r'], !.
whitespace --> ['\t'].
 
newline --> ['\n'].

comment_tail --> ['\n'], !.
comment_tail --> [_], !, comment_tail.
comment_tail --> [].

alphanum([Char | Chars], Len) --> 
    alphanum_char(Char), 
    !, 
    alphanum(Chars, L0), 
    {
        Len is L0 + 1 
    }.
alphanum([],  0) --> [].

signed_digit_seq(['-', D | Digits], Len) --> 
    ['-'], 
    !, 
    digit(D), 
    digit_seq_tail(Digits, TLen), 
    { Len is 2 + TLen }.
signed_digit_seq([D | Digits], Len) -->
    digit(D), 
    digit_seq_tail(Digits, TLen), 
    { Len is 1 + TLen }.

exponent([E | Tail], Len) --> 
    [E], 
    { member(E, [e, 'E']) }, 
    !, 
    signed_digit_seq(Tail, TLen), 
    { Len is 1 + TLen }.
exponent([], 0) --> [].

fraction_tail(['.' | Tail], Len) --> 
    ['.'], 
    !, 
    digit(D), 
    digit_seq_tail(DTail, DLen), 
    exponent(E, ELen), 
    {
        append([D | DTail], E, Tail),
        Len is 2 + DLen + ELen
    }.
fraction_tail(E, Len) --> exponent(E, Len), !.
fraction_tail([], 0) --> [].

digit_seq_tail([Digit | DTail], Len) --> 
    digit(Digit), 
    !, 
    digit_seq_tail(DTail, TLen), 
    { Len is TLen + 1 }.
digit_seq_tail([], 0) --> [].

special(Char) --> 
    [Char],
    {
        member(
            Char, 
            ['-', '+', '*', '/', '=', '>', '<', '·',
             '≠', '≤', '≥', '»', '«',
             '!', '@', '%', '^', '~', '&', '$', '|']
        )
    }.

classify_token(Atom, keyword(Atom)) :-
    member(
        Atom,
        [
            let, and, in, if, 
            then, match, else, 
            with, type, import, 
            define, '_', defop, 
            end, case
        ]
    ),
    !.
classify_token(true, bool(true)) :-
    !.
classify_token(false, bool(false)) :-
    !.
classify_token(Atom, id(Atom)).

classify_number(Digits, [], int(N)) :-
    atomic_list_concat(Digits, NumAtom),
    atom_number(NumAtom, N),
    !.
classify_number(Digits, Fractional, float(X)) :-
    append(Digits, Fractional, Number),
    atomic_list_concat(Number, NumAtom),
    atom_number(NumAtom, X).

char('\\', 2) --> ['\\', '\\'], !.
char('\n', 2) --> ['\\', 'n'], !.
char('\b', 2) --> ['\\', 'b'], !.
char('\f', 2) --> ['\\', 'f'], !.
char('\a', 2) --> ['\\', 'a'], !.
char('\r', 2) --> ['\\', 'r'], !.
char('\t', 2) --> ['\\', 't'], !.
char('\0', 2) --> ['\\', '0'], !.
char('"', 2)  --> ['\\', '"'], !.
char('\'', 2) --> ['\\', '\''], !.
char('', 0)   --> ['\\'], !, { fail }.
char('', 0)   --> ['\''], !, { fail }.
char('', 0)   --> ['"'],  !, { fail }.
char('', 0)   --> newline, !, { fail }.
char(Char, 1) --> [Char].

char_sequence([Char | Chars], N) -->
    char(Char, CLen),
    !,
    char_sequence(Chars, M),
    { N is CLen + M }.
char_sequence([], 0) --> [].

text_literal(char(C), Len) --> 
    char_delimiter, 
    !, 
    char(C, CLen),
    char_delimiter,
    { Len is CLen + 2 }.
text_literal(string(Str), Len) -->
    string_delimiter,
    !,
    char_sequence(StrList, StrLen),
    string_delimiter,
    { 
        Len is StrLen + 2,
        atomic_list_concat(StrList, Str)
    }.

continuous_sequence([]) --> whitespace, !.
continuous_sequence([]) --> newline, !.
continuous_sequence([X | Xs]) --> [X], !, continuous_sequence(Xs).
continuous_sequence([]) --> [].

operator_tail([OpHead | OpTail], N) --> 
    special(OpHead), 
    !, 
    operator_tail(OpTail, M), 
    { N is M + 1 }.
operator_tail([], 0) --> [].

delimiter('[') --> ['['], !.
delimiter(']') --> [']'], !.
delimiter('{') --> ['{'], !.
delimiter('}') --> ['}'], !.
delimiter('(') --> ['('], !.
delimiter(')') --> [')'], !.
delimiter(',') --> [','], !.
delimiter(':') --> [':'], !.
delimiter(';') --> [';'], !.
delimiter(',') --> ['.'].

lexer(Tokens, pos(F, L, C)) -->
    whitespace, 
    !, 
    { NC is C + 1 }, 
    lexer(Tokens, pos(F, L, NC)).
lexer(Tokens, pos(F, L, _)) --> 
    newline, 
    !, 
    { NL is L + 1 }, 
    lexer(Tokens, pos(F, NL, 1)).
lexer([Delimiter at pos(F, L, C) | Tokens], pos(F, L, C)) -->
    delimiter(Delimiter),
    !,
    { NC is C + 1 },
    lexer(Tokens, pos(F, L, NC)).
lexer(Tokens, pos(F, L, _)) -->
    ['#'], 
    !, 
    comment_tail, 
    { NL is L + 1 }, 
    lexer(Tokens, pos(F, NL, 1)).
lexer([op(Op) at pos(F, L, C) | Tokens], pos(F, L, C)) --> 
    special(OpHead), 
    !, 
    operator_tail(OpTail, Len), 
    { 
        NC is C + Len + 1,
        atomic_list_concat([OpHead | OpTail], Op)
    }, 
    lexer(Tokens, pos(F, L, NC)).
lexer([tid(TId) at pos(F, L, C) | Tokens], pos(F, L, C)) -->
    uppercase(Letter), 
    !, 
    alphanum(Tail, Len), 
    { NC is C + Len + 1 },
    lexer(Tokens, pos(F, L, NC)), 
    { atomic_list_concat([Letter | Tail], TId) }.
lexer([Token at pos(F, L, C) | Tokens], pos(F, L, C)) -->
    lowercase(Letter), 
    !, 
    alphanum(Tail, Len), 
    { NC is C + Len + 1 }, 
    lexer(Tokens, pos(F, L, NC)), 
    {
        atomic_list_concat([Letter | Tail], Atom),
        classify_token(Atom, Token)
    }.
lexer([Num at pos(F, L, C) | Tokens], pos(F, L, C)) -->
    digit(D), 
    !, 
    digit_seq_tail(Digits, Len), 
    fraction_tail(FTail, FLen), 
    {
        NC is C + Len + FLen + 1,
        classify_number([D | Digits], FTail, Num)
    }, 
    lexer(Tokens, pos(F, L, NC)).
lexer([char(Char) at pos(F, L, C) | Tokens], pos(F, L, C)) -->
    char_delimiter,
    !,
    char_literal(Char, Len, pos(F, L, C)),
    {
        NC is C + Len + 2 
    },
    lexer(Tokens, pos(F, L, NC)).  
lexer([string(Str) at pos(F, L, C) | Tokens], pos(F, L, C)) -->
    string_delimiter,
    !,
    char_sequence(StrChars, Len),
    { 
        NC is C + Len + 2,
        atomic_list_concat(StrChars, Str)
    },
    string_terminator(Str, pos(F, L, C)),
    lexer(Tokens, pos(F, L, NC)).
lexer(_, Pos) -->
    [X],
    !,
    continuous_sequence(Characters),
    { 
        atomic_list_concat([X | Characters], Err),
        throw(error('invalid token ~w', [Err]) at Pos)
    }.
lexer([], _) --> [].
