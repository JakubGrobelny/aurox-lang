:- ensure_loaded(utility).
:- ensure_loaded(environment).


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
infer_type(Env, if(Cond, Then, Else), T, Start, FinalEnv) :-
    infer_type(Env, Cond, CondT, Start, CondEnv),
    infer_type(CondEnv, Then, ThenT, Start, ThenEnv),
    infer_type(ThenEnv, Else, ElseT, Start, FinalEnv),
    typecheck_if(CondT, ThenT, ElseT, Start, T).
infer_type(Env, Logical, adt('Bool', []), Start, FinalEnv) :-
    Logical =.. [Op, Lhs, Rhs],
    member(Op, [and, or]),
    !,
    infer_type(Env, Lhs, LhsT, Start, NewEnv),
    infer_type(NewEnv, Rhs, RhsT, Start, FinalEnv),
    typecheck_logical(LhsT, RhsT, Start).


% TODO: add more cases

typecheck_logical( adt('Bool', []), adt('Bool', []), _) :- !.
typecheck_logical( Rhs, Lhs, Start) :-
    print_type_error(
        Start,
        'operator ~w type mismatch in logical operator arguments.\c
         Expected Bool and Bool, got ~w and ~w',
        [Lhs, Rhs]
    ).

typecheck_if(adt('Bool', []), T, T, _, T) :- !.
typecheck_if(adt('Bool', []), ThenT, ElseT, Start, _) :-
    print_type_error(
        Start,
        'type mismatch in if expression.\c
         Type ~w of consequence does not match the type ~w of alternative',
        [ThenT, ElseT]
    ).
typecheck_if(CondT, _, _, Start, _) :-
    print_type_error(
        Start,
        'invalid type of conditional expression.\c
         Expected Bool, got ~w',
        [CondT]
    ).

typecheck_application(A->B, A, _, B) :- !.
typecheck_application(FunT, ArgT, Start, _) :-
    print_type_error(
        Start,
        'type mismatch in function application.\c
         Expression of type ~w can\'t be applied to expression of type ~w',
         [FunT, ArgT]
    ).

typecheck_list(Env, [], _, _, Env) :- !.
typecheck_list(Env, [H | T], Type, Start, FinalEnv) :-
    infer_type(Env, H, Type, Start, NewEnv),
    !,
    typecheck_list(NewEnv, T, Type, Start, FinalEnv).
typecheck_list(Env, [H| _], Type, Start, _) :-
    infer_type(Env, H, HType, Start, _),
    print_type_error(
        Start,
        'type mismatch in a list.\c 
         Expected [~w] but it contains element of type ~w',
         [Type, HType]
    ).

print_type_error(Start, MsgFormat, MsgArgs) :-
    prettify_types(MsgArgs, Prettified),
    print_error_and_halt(Start, MsgFormat, Prettified).

prettify_types([], []) :- !.
prettify_types([T | Ts], [PT | PTs]) :-
    prettify_type(T, PT),
    prettify_types(Ts, PTs).

prettify_type(X, X) :- !. % TODO: implement

extract_variables_from_type(none, []) :- !.
extract_variables_from_type(Var, []) :-
    var(Var),
    !.
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
