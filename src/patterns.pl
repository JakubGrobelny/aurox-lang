:- ensure_loaded(utility).

remove_pattern_vars(Pattern, FixedPattern) :-
    dict_create(Map, vars, []),
    remove_pattern_vars(Pattern, Map, _, FixedPattern).

remove_pattern_vars(id(A), Map, Map, AVar) :-
    get_dict(A, Map, AVar),
    !.
remove_pattern_vars(id(A), Map, NewMap, AVar) :-
    put_dict(A, Map, AVar, NewMap).
remove_pattern_vars(adt(_, Arg), Map, NewMap, Fixed) :-
    remove_pattern_vars(Arg, Map, NewMap, Fixed).
remove_pattern_vars(list(Xs), Map, NewMap, list(Fixed)) :-
    remove_pattern_vars_list(Xs, Map, NewMap, Fixed).
remove_pattern_vars(tuple(N, Elements), Map, NewMap, tuple(N, Fixed)) :-
    list_of_tuple(Elements, List),
    remove_pattern_vars_list(List, Map, NewMap, Fixed).

remove_pattern_vars_list([], Map, Map, []) :- !.
remove_pattern_vars_list([H | T], Map, FinalMap, [FH | FT]) :-
    remove_pattern_vars(H, Map, NewMap, FH),
    remove_pattern_vars_list(T, NewMap, FinalMap, FT).
