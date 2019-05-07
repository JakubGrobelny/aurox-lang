import_core_definitions(CoreEnv) :-
    dict_create(
        CoreEnv,
        globenv,
        [
            '__add':(
                bfun('__add'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            ),
            '__sub':(
                bfun('__sub'),
                (adt('Int', [])->adt('Int', [])->adt('Int', [])),
                builtin
            )
        ]
    ).

'__add'(tuple(_, (L, R)), Sum) :-
    Sum is L + R.
'__sub'(tuple(_, (L, R)), Sum) :-
    Sum is L - R.










