:- ensure_loaded(utility).
:- ensure_loaded(ffi).

import_core_definitions(CoreEnv) :-
    dict_create(
        CoreEnv,
        globenv,
        [
            '`types':types{
                'Int'  : (0, builtin),
                'Bool' : (0, builtin),
                'Char' : (0, builtin),
                'Void' : (0, builtin),
                'Float': (0, builtin),
                'Unit' : (0, builtin),
                'Mutable' : (1, builtin)
            },
            '__add':(
                bfun('__add') at builtin,
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Int', [])),
                builtin
            ),
            '__sub':(
                bfun('__sub') at builtin,
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Int', [])),
                builtin
            ),
            '__mult':(
                bfun('__mult') at builtin,
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Int', [])),
                builtin
            ),
            '__div':(
                bfun('__div') at builtin,
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Maybe', [adt('Int', [])])),
                builtin
            ),
            '__mod':(
                bfun('__mod') at builtin,
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Maybe', [adt('Int', [])])),
                builtin
            ),
            '__pow':(
                bfun('__pow') at builtin,
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Int', [])),
                builtin
            ),
            '__neg':(
                bfun('__neg') at builtin,
                (adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__addf':(
                bfun('__add') at builtin,
                (tuple(2, (adt('Float', []), adt('Float', [])))->adt('Float', [])),
                builtin
            ),
            '__subf':(
                bfun('__sub') at builtin,
                (tuple(2, (adt('Float', []), adt('Float', [])))->adt('Float', [])),
                builtin
            ),
            '__multf':(
                bfun('__mult') at builtin,
                (tuple(2, (adt('Float', []), adt('Float', [])))->adt('Float', [])),
                builtin
            ),
            '__divf':(
                bfun('__divf') at builtin,
                (tuple(2, (adt('Float', []), adt('Float', [])))->adt('Float', [])),
                builtin
            ),
            '__powf':(
                bfun('__pow') at builtin,
                (tuple(2, (adt('Float', []), adt('Float', [])))->adt('Float', [])),
                builtin
            ),
            '__negf':(
                bfun('__neg') at builtin,
                (adt('Float', [])->adt('Float', [])),
                builtin
            ),
            '__cmp_less':(
                bfun('__cmp_less') at builtin,
                (tuple(2, (param(a), param(a)))->adt('Bool', [])),
                builtin
            ),
            '__cmp_great':(
                bfun('__cmp_great') at builtin,
                (tuple(2, (param(a), param(a)))->adt('Bool', [])),
                builtin
            ),
            '__cmp_eq':(
                bfun('__cmp_eq') at builtin,
                (tuple(2, (param(a), param(a)))->adt('Bool', [])),
                builtin
            ),
            '__cmp_less_eq':(
                bfun('__cmp_less_eq') at builtin,
                (tuple(2, (param(a), param(a)))->adt('Bool', [])),
                builtin
            ),
            '__cmp_great_eq':(
                bfun('__cmp_great_eq') at builtin,
                (tuple(2, (param(a), param(a)))->adt('Bool', [])),
                builtin
            ),
            '__cmp_neq':(
                bfun('__cmp_neq') at builtin,
                (tuple(2, (param(a), param(a)))->adt('Bool', [])),
                builtin
            ),
            '__not':(
                bfun('__not') at builtin,
                (adt('Bool', [])->adt('Bool', [])),
                builtin
            ),
            '__int_to_float':(
                bfun('__int_to_float') at builtin,
                (adt('Int', [])->adt('Float', [])),
                builtin
            ),
            '__float_to_int':(
                bfun('__float_to_int') at builtin,
                (adt('Float', [])->adt('Int', [])),
                builtin
            ),
            '__show':(
                bfun('__show') at builtin,
                (param(a)->list(adt('Char', []))),
                builtin
            ),
            '__print':(
                bfun('__print') at builtin,
                (list(adt('Char', []))->adt('Unit', [])),
                builtin
            ),
            '__read_int':(
                bfun('__read_int') at builtin,
                (adt('Unit', [])->adt('Maybe', [adt('Int', [])])),
                builtin
            ),
            '__read_float':(
                bfun('__read_float') at builtin,
                (adt('Unit', [])->adt('Maybe', [adt('Float', [])])),
                builtin
            ),
            '__head':(
                bfun('__head') at bultin,
                (list(param(a))->param(a)),
                builtin
            ),
            '__tail':(
                bfun('__tail') at builtin,
                (list(param(a))->list(param(a))),
                builtin
            ),
            '__null':(
                bfun('__null') at builtin,
                (list(param(a))->adt('Bool', [])),
                builtin
            ),
            '__sort':(
                bfun('__sort') at builtin,
                (list(param(a))->list(param(a))),
                builtin
            ),
            '__char_code':(
                bfun('__char_code') at builtin,
                (adt('Char', [])->adt('Int', [])),
                builtin
            ),
            '__exit':(
                bfun('__exit') at builtin,
                (adt('Int', [])->adt('Unit', [])),
                builtin
            ),
            '__read_string':(
                bfun('__read_string') at bultin,
                (adt('Unit', [])->adt('Maybe', [list(adt('Char', []))])),
                builtin
            ),
            '__read_char':(
                bfun('__read_char') at bultin,
                (adt('Unit', [])->adt('Char', [])),
                builtin
            ),
            '__make_mut':(
                bfun('__make_mut') at builtin,
                (param(a)->adt('Mutable', [param(a)])),
                builtin
            ),
            '__unwrap_mut':(
                bfun('__unwrap_mut') at builtin,
                (adt('Mutable', [param(a)])->param(a)),
                builtin
            ),
            '__update_mut':(
                bfun('__update_mut') at builtin,
                (
                    tuple(2, (adt('Mutable', [param(a)]), param(a)))
                    ->
                    adt('Unit', [])
                ),
                builtin
            ),
            '__clone':(
                bfun(duplicate_term) at builtin,
                (param(a)->param(a)),
                builtin
            ),
            '__unsafe_unwrap_mut':(
                bfun('__unsafe_unwrap_mut') at builtin,
                (adt('Mutable', [param(a)])->param(a)),
                builtin
            )
        ]
    ).

'__add'((L, R), Res) :-
    Res is L + R.

'__sub'((L, R), Res) :-
    Res is L - R.

'__mult'((L, R), Res) :-
    Res is L * R.

'__div'((_, 0), 'Nothing') :- !.
'__div'((L, R), 'Just'/Res) :-
    Res is L div R.

'__mod'((_, 0), 'Nothing') :- !.
'__mod'((L, R), 'Just'/Res) :-
    Res is L mod R.

'__pow'((L, R), Res) :-
    Res is L ** R.

'__neg'(N, Res) :-
    Res is -N.

'__divf'((_, 0.0), _) :-
    !,
    throw(runtime_error(division_by_zero)).
'__divf'((L, R), Res) :-
    Res is L / R.

'__cmp_less'((L, R), true) :-
    L @< R,
    !.
'__cmp_less'((_, _), false).

'__cmp_great'((L, R), true) :-
    L @> R,
    !.
'__cmp_great'((_, _), false).

'__cmp_eq'((L, R), true) :-
    L == R,
    !.
'__cmp_eq'((_, _), false).

'__not'(true, false) :- !.
'__not'(false, true).

'__cmp_less_eq'((L, R), true) :-
    L @=< R,
    !.
'__cmp_less_eq'((_, _), false).

'__cmp_great_eq'((L, R), true) :-
    L @>= R,
    !.
'__cmp_great_eq'((_, _), false).

'__cmp_neq'((L, R), true) :-
    \+ L == R,
    !.
'__cmp_neq'((_, _), false).

'__float_to_int'(Float, Int) :-
    Int is truncate(Float).

'__int_to_float'(Int, Float) :-
    Float is float(Int).

'__show'(Val, CharList) :-
    prettify_expr(Val, Prettified),
    term_to_atom(Prettified, Atom),
    atom_chars(Atom, CharList).

'__print'(Chars, unit) :-
    atomic_list_concat(Chars, Str),
    current_output(Out),
    format(Str),
    flush_output(Out).

'__read_int'(_, 'Just'/Int) :-
    % read(Int),
    % integer(Int),
    io:read_int(Int),
    !.
'__read_int'(_, 'Nothing').

'__read_float'(_, 'Just'/Float) :-
    % read(Float),
    % float(Float),/*  */
    io:read_float(Float),
    !.
'__read_float'(_, 'Nothing').

'__head'([X|_], X) :- !.
'__head'([], _) :-
    throw(runtime_error('head: empty list')).

'__tail'([_ | Xs], Xs) :- !.
'__tail'([], _)  :-
    throw(runtime_error('tail: empty list')).

'__null'([], true) :- !.
'__null'(_, false).

'__sort'(Xs, Ys) :-
    msort(Xs, Ys).

'__char_code'(C, Code) :-
    char_code(C, Code).

'__exit'(Code, unit) :-
    halt(Code).

'__read_string'(_, 'Just'/CharList) :-
    io:read_string(Str),
    !,
    atom_chars(Str, CharList).
'__read_string'(_, 'Nothing').

'__read_char'(_, C) :-
    io:read_char(C).

'__make_mut'(Val, 'Mut'/Copy) :-
    copy_term(Val, Copy).

'__unwrap_mut'('Mut'/Val, Copy) :-
    copy_term(Val, Copy).

'__update_mut'((Mutable, Val), unit) :-
    setarg(2, Mutable, Val).

'__unsafe_unwrap_mut'('Mut'/Mutable, Mutable).

