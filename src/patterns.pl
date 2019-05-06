:- ensure_loaded(utility).

fix_pattern(Pattern, FixedPattern) :-
    dict_create(Map, vars, []),
    fix_pattern(Pattern, Map, _, FixedPattern).

fix_pattern(id(A), Map, Map, AVar) :-
    get_dict(A, Map, AVar),
    !.
fix_pattern(id(A), Map, NewMap, AVar) :-
    put_dict(A, Map, AVar, NewMap).
fix_pattern(adt(_, Arg), Map, NewMap, Fixed) :-
    fix_pattern(Arg, Map, NewMap, Fixed).
fix_pattern(list(Xs), Map, NewMap, list(Fixed)) :-
    fix_pattern_list(Xs, Map, NewMap, Fixed).
fix_pattern(tuple(N, Elements), Map, NewMap, tuple(N, Fixed)) :-
    list_of_tuple(Elements, List),
    fix_pattern_list(List, Map, NewMap, Fixed).

fix_pattern_list([], Map, Map, []) :- !.
fix_pattern_list([H | T], Map, FinalMap, [FH | FT]) :-
    fix_pattern(H, Map, NewMap, FH),
    fix_pattern_list(T, NewMap, FinalMap, FT).
