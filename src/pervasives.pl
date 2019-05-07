import_core_definitions(CoreEnv) :-
    dict_create(
        CoreEnv,
        globenv,
        [
            '`types':types{
                'Int'  :0,
                'Bool' :0,
                'Char' :0,
                'Void' :0,
                'Float':0
            },
            '__add':(
                bfun('__add'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__sub':(
                bfun('__sub'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__mult':(
                bfun('__mult'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__div':(
                bfun('__div'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__pow':(
                bfun('__pow'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__neg':(
                    bfun('__neg'),
                (adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__addf':(
                bfun('__addf'),
                (adt('Float', [])->adt('Float', [])->adt('Float', [])),
                builtin
            ),
            '__subf':(
                bfun('__subf'),
                (adt('Float', [])->adt('Float', [])->adt('Float', [])),
                builtin
            ),
            '__multf':(
                bfun('__multf'),
                (adt('Float', [])->adt('Float', [])->adt('Float', [])),
                builtin
            ),
            '__divf':(
                bfun('__divf'),
                (adt('Float', [])->adt('Float', [])->adt('Float', [])),
                builtin
            ),
            '__powf':(
                bfun('__powf'),
                (adt('Float', [])->adt('Float', [])->adt('Float', [])),
                builtin
            ),
            '__negf':(
                bfun('__negf'),
                (adt('Float', [])->adt('Float', [])),
                builtin
            ),

            '__cmp_less':(
                bfun('__cmp_less'),
                (param(a)->param(a)->adt('Bool', [])),
                builtin
            ),
            '__cmp_great':(
                    bfun('__cmp_great'),
                    (param(a)->param(a)->adt('Bool', [])),
                    builtin
            ),
            '__cmp_eq':(
                bfun('__cmp_eq'),
                (param(a)->param(a)->adt('Bool', [])),
                builtin
            ),
            '__cmp_less_eq':(
                bfun('__cmp_less_eq'),
                (param(a)->param(a)->adt('Bool', [])),
                builtin
            ),
            '__cmp_great_eq':(
                    bfun('__cmp_great_eq'),
                    (param(a)->param(a)->adt('Bool', [])),
                    builtin
            ),
            '__cmp_neq':(
                bfun('__cmp_neq'),
                (param(a)->param(a)->adt('Bool', [])),
                builtin
            ),
            '__not':(
                bfun('__not'),
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

'__addf'((L, R), Res) :-
    Res is L + R.

'__subf'((L, R), Res) :-
    Res is L - R.

'__multf'((L, R), Res) :-
    Res is L * R.

'__divf'((_, 0.0), _) :-
    !,
    throw(runtime_error(division_by_zero)).
'__divf'((L, R), Res) :-
    Res is L / R.

'__powf'((L, R), Res) :-
    Res is L ** R.

'__negf'(N, Res) :-
    Res is -N.

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







