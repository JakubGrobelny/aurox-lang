:- ensure_loaded(utility).

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
                'Unit' : (0, builtin)
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
                (tuple(2, (adt('Int', []), adt('Int', [])))->adt('Int', [])),
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
            )
        ]
    ).

'__add'((L, R), Res) :-
    Res is L + R.

'__sub'((L, R), Res) :-
    Res is L - R.

'__mult'((L, R), Res) :-
    Res is L * R.

'__div'((_, 0), _) :-
    !,
    throw(runtime_error(division_by_zero)).
'__div'((L, R), Res) :-
    Res is L div R.

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

