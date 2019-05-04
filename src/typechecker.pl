:- ensure_loaded(utility).

infer_type(Env, int(_), adt('Int', []), _, Env) :- !.
infer_type(Env, float(_), adt('Float', []), _, Env) :- !.
infer_type(Env, unit, adt('Unit', []), _, Env) :- !.
infer_type(Env, bool(_), adt('Bool', []), _, Env) :- !.
infer_type(Env, char(_), adt('Char', []), _, Env) :- !.
infer_type(Env, list([]), list(_), _, Env) :- !.
infer_type(Env, list([H | T]), list(Type), Start, FinalEnv) :-
    infer_type(Env, H, Type, Start, NewEnv),
    typecheck_list(NewEnv, T, Type, Start, FinalEnv).
infer_type(Env, app(A, B), T, Start, FinalEnv) :-
    infer_type(Env, A, TA, Start, NewEnv),
    infer_type(NewEnv, B, TB, Start, FinalEnv),
    typecheck_application(TA, TB, Start, T).
% TODO: add more cases

typecheck_application(A->B, A, _, B) :- !.
typecheck_application(FunT, ArgT, Start) :-
    prettify_type(FunT, PFunT),
    prettify_type(ArgT, PArgT),
    print_error_and_halt(
        Start,
        'type mismatch in function application.\c
         Expression of type ~w can\'t be applied to expression of type ~w',
         [PFunT, PArgT]
    ).

typecheck_list(Env, [], _, _, Env) :- !.
typecheck_list(Env, [H | T], Type, Start, FinalEnv) :-
    infer_type(Env, H, Type, Start, NewEnv),
    !,
    typecheck_list(NewEnv, T, Type, Start, FinalEnv).
typecheck_list(Env, [H| _], Type, Start, _) :-
    infer_type(Env, H, HType, Start, _),
    prettify_type(Type, PType),
    prettify_type(HType, PHType),
    print_error_and_halt(
        Start,
        'type mismatch in a list.\c 
         Expected [~w] but it contains element of type ~w',
         [PType, PHType]
    ).

prettify_type(X, X) :- !. % TODO: implement

extract_variables_from_type(undefined, []) :- !.
extract_variables_from_type(param(A), [A]) :- !.
extract_variables_from_type(name(_), []) :- !.
extract_variables_from_type(adt(_, Parameters), FilteredParams) :-
    !,
    extract_variables_from_type(Parameters, FilteredParams).
extract_variables_from_type([T | Types], Params) :-
    !,
    extract_variables_from_type(T, TParams), 
    extract_variables_from_type(Types, RestParams),
    append(TParams, RestParams, Params).
extract_variables_from_type([], []) :- !.
extract_variables_from_type(tuple(_, Types), Params) :-
    !,
    extract_variables_from_type(Types, Params).
extract_variables_from_type(T, Params) :-
    T =.. [_ | Args],
    extract_variables_from_type(Args, Params).
