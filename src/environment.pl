:- ensure_loaded(utility).
:- ensure_loaded(typechecker).
:- ensure_loaded(operators).

empty_env(Env) :-
    dict_create(Env, globenv, []).

empty_type_env(TEnv) :-
    dict_create(TEnv, tenv, []).

add_definitions_to_env([], Env, Env, []) :- !.
add_definitions_to_env(
    [define(Name, T, V) at Where | Program], 
    EnvIn, 
    EnvOut,
    ProgRest
) :-
    !,
    Name =.. [_, N],
    redefinition_warning(N, EnvIn, Where),
    env_add(Name, EnvIn, V, T, Where, EnvIntermediate),
    add_definitions_to_env(Program, EnvIntermediate, EnvOut, ProgRest).
add_definitions_to_env([P| Prog], Env, EnvOut, [P | ProgRest]) :-
    add_definitions_to_env(Prog, Env, EnvOut, ProgRest).

env_add(op(Operator), Env, lambda([Arg], Val), TSig, Place, NewEnv) :-
    !,
    mark_unary_operator(Operator, UnaryOperator),
    put_dict(UnaryOperator, Env, (lambda([Arg], Val), TSig, Place), NewEnv).
env_add(Name, Env, Value, TypeSignature, Place, NewEnv) :-
    Name =.. [_, N],
    put_dict(N, Env, (Value, TypeSignature, Place), NewEnv).

redefinition_warning(Name, Env, Where) :-
    get_dict(Name, Env, (_, _, pos(F, L, C))),
    !,
    fix_name(Name, FixedName),
    print_warning(
        Where, 
        'redefinition of ~w, previously defined at ~w:~w:~w',
        [FixedName, F, L, C]
    ).
redefinition_warning(_, _, _).

fix_name(UnOp, Op) :-
    atomic_list_concat(['`' | Op], ' ', UnOp),
    !.
fix_name(Id, Id).

add_typedefs_to_env(Env, [], TEnv, Env, [], TEnv) :- !.
add_typedefs_to_env(
    Env,
    [typedef(Name, Params, Constructors) at Place | Program],
    TEnv,
    FinalEnv,
    FinalProgram,
    FinalTEnv
) :-
    !,
    add_type_to_type_env(Name, Params, Constructors, TEnv, Place, NewTEnv),
    add_constructors_to_env(Constructors, Place, Env, NewEnv),
    add_typedefs_to_env(
        NewEnv,
        Program,
        NewTEnv,
        FinalEnv,
        FinalProgram,
        FinalTEnv
    ).
add_typedefs_to_env(
    Env, 
    [P | Prog], 
    TEnv, 
    FinalEnv, 
    [P | FinalProg], 
    FinalTEnv
) :-
    add_typedefs_to_env(Env, Prog, TEnv, FinalEnv, FinalProg, FinalTEnv).

add_type_to_type_env(Name, _, _, TEnv, Place, _) :-
    get_dict(Name, TEnv, (_, _, pos(File, L, C))),
    !,
    print_error_and_halt(
        Place, 
        'redefinition of type ~w, previously defined at ~w:~w:~w', 
        [Name, File, L, C]
    ).
add_type_to_type_env(Name, Params, Constructors, TEnv, Place, NewTEnv) :-
    valid_param_list(Params, Place, SortedParams),
    check_parameter_occurence(SortedParams, Constructors, Place),
    put_dict(Name, TEnv, (Params, Constructors), NewTEnv).

valid_param_list(Param, _, Sorted) :-
    sort(Param, Sorted),
    msort(Param, Sorted),
    !.
valid_param_list(_, Place, _) :-
    print_error_and_halt(
        Place,
        'invalid parameter list (contains duplicates)',
        []
    ).

check_parameter_occurence(Params, Constructors, _) :-
    extract_params_constructors(Constructors, ConsParamList),
    sort(ConsParamList, Params),
    !.
check_parameter_occurence(_, _, Place) :-
    print_error_and_halt(
        Place,
        'free type variable in type definition',
        []
    ).

extract_params_constructors([], []) :- !.
extract_params_constructors([constructor(_, T) | Constrs], Params) :-
    extract_variables_from_type(T, ConstructorParams),
    extract_params_constructors(Constrs, ParamsTail),
    append(ConstructorParams, ParamsTail, Params).

add_constructors_to_env([], _, Env, Env) :- !.
add_constructors_to_env([Cons | Constrs], Place, Env, FinalEnv) :-
    Cons =.. [_, CName, undefined],
    !,
    add_cons_to_env(CName, Env, enum(CName), Place, NewEnv),
    add_constructors_to_env(Constrs, Place, NewEnv, FinalEnv).
add_constructors_to_env([Cons | Constrs], Place, Env, FinalEnv) :-
    Cons =.. [_, CName, _],
    lambda_from_constructor(CName, Lambda, Place),
    add_cons_to_env(CName, Env, Lambda, Place, NewEnv),
    add_constructors_to_env(Constrs, Place, NewEnv, FinalEnv).

lambda_from_constructor(Cons, lambda([x], adt(Cons, id(x) at Place)), Place).

add_cons_to_env(Name, Env, _, Where, _) :-
    get_dict(Name, Env, (_, _, pos(F, L, C))),
    !,
    print_error_and_halt(
        Where, 
        'redefinition of constructor ~w, previously defined at ~w:~w:~w',
        [Name, F, L, C]
    ).
add_cons_to_env(Name, Env, Val, Where, NewEnv) :-
    put_dict(Name, Env, (Val, undefined, Where), NewEnv).



