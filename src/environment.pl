:- ensure_loaded(utility).
:- ensure_loaded(typechecker).
:- ensure_loaded(operators).

empty_env(Env) :-
    dict_create(Types, types, []),
    dict_create(Env, globenv, ['`types':Types]).

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

env_add(op(Operator), Env, lambda(Arg, Val), TSig, Place, NewEnv) :-
    !,
    mark_unary_operator(Operator, UnaryOperator),
    % fix_type_signature(TSig, NewSig),
    put_dict(UnaryOperator, Env, (lambda(Arg, Val), TSig, Place), NewEnv).
env_add(Name, Env, Value, TypeSignature, Place, NewEnv) :-
    Name =.. [_, N],
    % fix_type_signature(TypeSignature, NewSig),
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

get_variable_value(Var, Env, _, Val) :-
    get_dict(Var, Env, Val),
    !.
get_variable_value(Var, _, Start, _) :-
    print_error_and_halt(Start,'Undefined variable ~w',[Var]).

fix_name(UnOp, Op) :-
    atomic_list_concat(['`' | Op], ' ', UnOp),
    !.
fix_name(Id, Id).

valid_typedef(_, SortedParams, Constructors, _) :-
    extract_params_constructors(Constructors, ConsParamList),
    sort(ConsParamList, SortedParams),
    !.
valid_typedef(Name, _, _, Place) :-
    print_error_and_halt(
        Place,
        'type ~w contains free type variable in its definition',
        [Name]
    ).

extract_params_constructors([], []) :- !.
extract_params_constructors([constructor(_, T) | Constrs], Params) :-
    extract_variables_from_type(T, ConstructorParams),
    extract_params_constructors(Constrs, ParamsTail),
    append(ConstructorParams, ParamsTail, Params).

params_to_keys([], []) :- !.
params_to_keys([Param | Params], [Param:_ | Vars]) :-
    params_to_keys(Params, Vars).

params_to_vars([], _, []) :- !.
params_to_vars([P | Ps], Map, [V | Vs]) :-
    get_dict(P, Map, V),
    params_to_vars(Ps, Map, Vs).

add_typedefs_to_env([], Env, Env, []) :- !.
add_typedefs_to_env(
    [typedef(Name, Params, Constructors) at Pos | Program],
    Env,
    FinalEnv,
    FinalProgram
) :-
    !,
    msort(Params, SortedParams),
    valid_typedef(Name, SortedParams, Constructors, Pos),
    params_to_keys(SortedParams, ParamKeys),
    dict_create(ParamMap, params, ParamKeys),
    params_to_vars(Params, ParamMap, Vars),
    add_constructors_to_env(
        adt(Name, Vars), 
        Constructors, 
        ParamMap, 
        Pos, 
        Env, 
        NewEnv
    ),
    length(Params, NParams),
    add_type_to_env(Name, NParams, NewEnv, EnvWithTypes, Pos),
    add_typedefs_to_env(Program, EnvWithTypes, FinalEnv, FinalProgram).
add_typedefs_to_env([_ | Program], Env, FinalEnv, FinalProgram) :-
    add_typedefs_to_env(Program, Env, FinalEnv, FinalProgram).

add_type_to_env(Name, NParams, Env, NewEnv, Pos) :-
    get_dict('`types', Env, Types),
    add_type_to_env_and_check(Name, NParams, Types, NewTypes, Pos),
    put_dict('`types', Env, NewTypes, NewEnv).
add_type_to_env_and_check(Name, _, Types, _, Pos) :-
    get_dict(Name, Types, (_, pos(F, L, C))),
    !,
    print_error_and_halt(
        Pos,
        'Redefinition of type ~w, previously defined at ~w:~w:~w',
        [Name, F, L, C]
    ).
add_type_to_env_and_check(Name, NParams, Types, NewTypes, Pos) :-
    put_dict(Name, Types, (NParams, Pos), NewTypes).

add_constructors_to_env(_, [], _, _, Env, Env) :- !.
add_constructors_to_env(Type, [C | Cs], VarMap, Pos, Env, FinalEnv) :-
    C =.. [_, CName, none],
    !,
    add_cons_to_env(CName, Type, enum(CName) at Pos, Pos, Env, NewEnv),
    add_constructors_to_env(Type, Cs, VarMap, Pos, NewEnv, FinalEnv).
add_constructors_to_env(Type, [C | Cs], VarMap, Pos, Env, FinalEnv) :-
    C =.. [_, CName, Args],
    map_variable_to_type(Args, VarMap, MappedArgs),
    add_cons_to_env(
        CName, 
        MappedArgs->Type,
        lambda(arg, adt(CName, id(arg))) at Pos,
        Pos,
        Env,
        NewEnv
    ),
    add_constructors_to_env(Type, Cs, VarMap, Pos, NewEnv, FinalEnv).

add_cons_to_env(Name, _, _, Pos, Env, _) :-
    get_dict(Name, Env, (_, _, pos(F, L, C))),
    !,
    print_error_and_halt(
        Pos, 
        'redefinition of constructor ~w, previously defined at ~w:~w:~w',
        [Name, F, L, C]
    ).
add_cons_to_env(Name, Type, Val, Pos, Env, NEnv) :-
    put_dict(Name, Env, (Val, Type, Pos), NEnv).