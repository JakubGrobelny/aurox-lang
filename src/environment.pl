:- ensure_loaded(utility).
:- ensure_loaded(typechecker).

empty_environment(Env) :-
    dict_create(Env, globenv, []).

add_definitions_to_environment([], Env, Env) :- !.
add_definitions_to_environment(
    [define(N, T, V) at Where | Program], 
    EnvIn, 
    EnvOut
) :-
    (get_dict(N, EnvIn, (_, _, pos(F,L,C))) ->
        print_warning(
            Where, 
            'redefinition of ~w, previously defined at ~w:~w:~w',
            [N, F, L, C]
        ); 
        true
    ),
    put_dict(N, EnvIn, (V, T, Where), EnvIntermediate),
    add_definitions_to_environment(Program, EnvIntermediate, EnvOut).