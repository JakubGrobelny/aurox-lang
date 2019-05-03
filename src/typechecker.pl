

typecheck_environment(Env) :-
    dict_pairs(Env, _, Definitions),
    typecheck_environment(Env, Definitions, [], []).

% TODO: ???