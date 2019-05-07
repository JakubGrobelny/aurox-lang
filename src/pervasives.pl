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
            )
        ]
    ).

'__add'(tuple(_, (L, R)), Sum) :-
    Sum is L + R.
'__sub'(tuple(_, (L, R)), Sum) :-
    Sum is L - R.










