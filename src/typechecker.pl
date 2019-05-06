:- ensure_loaded(utility).
:- ensure_loaded(environment).

typecheck_environment(env(Env, TEnv)) :-
    dict_pairs(Env, _, Contents),
    typecheck_environment(Contents, env(Env, TEnv)).

% TODO: consider changing param(x) into X for every type
typecheck_environment([], _) :- !.
typecheck_environment([_-(Val at Pos, Type, _) | Vars], Env) :-
    infer_type(Env, Val, Type, Pos),
    \+ var(Type),
    !,
    typecheck_environment(Vars, Env).
% TODO: case when Var is a Constructor
typecheck_environment([Var-(Val at ValPos, Type, Pos) | _], Env) :-
    infer_type(Env, Val, ValType, Pos),
    \+ var(ValType),
    !,
    print_type_error(
        ValPos,
        'The type ~w of expression ~w does not match the type \c 
         annotation ~w specified in the definition of ~w',
        [type(ValType), Val, type(Type), Var]
    ).
typecheck_environment([Var-(_ at ValPos, _, _) | _], _) :-
    !,
    print_type_error(
        ValPos,
        'the type of value of ~w couldn\'t have been inferred',
        [Var]
    ).

infer_type(env(Env, _), id(Var), Type, _) :-
    get_dict(Var, Env, (var, PrevType, Pos)),
    \+ var(PrevType),
    PrevType = rec,
    !,
    b_set_dict(Var, Env, (var, Type, Pos)).
infer_type(env(Env, _), id(Var), Type, _) :-
    get_dict(Var, Env, (var, Type, _)),
    !.
infer_type(env(Env, _), id(Var), Type, _) :-
    get_dict(Var, Env, (Val, PrevType, DefPos)),
    \+ var(PrevType),
    PrevType = rec,
    !,
    b_set_dict(Var, Env, (Val, Type, DefPos)).
infer_type(env(Env, _), id(Var), ExpectedType, _) :-
    get_dict(Var, Env, (_, Type, _)),
    \+ var(Type),
    !,
    copy_term(Type, ExpectedType).
infer_type(env(Env, TEnv), id(Var), ExpectedType, _) :-
    get_dict(Var, Env, (Val at Pos, Type, DefPos)),
    b_set_dict(Var, Env, (Val at Pos, rec, DefPos)),
    !,
    infer_type(env(Env, TEnv), Val, InferredType, DefPos),
    typecheck_rec(InferredType, Type, Env, Var, Val at Pos, DefPos),
    copy_term(Type, ExpectedType).
infer_type(_, int(_), adt('Int', []), _) :- !.
infer_type(_, float(_), adt('Float', []), _) :- !.
infer_type(_, char(_), adt('Char', []), _) :- !.
infer_type(_, bool(_), adt('Bool', []), _) :- !.
infer_type(_, unit, adt('Unit', []), _) :- !.
infer_type(Env, app(Fun, Arg), T, Pos) :-
    infer_type(Env, Fun, FunT, Pos),
    infer_type(Env, Arg, ArgT, Pos),
    typecheck_application(FunT, ArgT, T, Pos),
    !.
infer_type(Env, Logical, adt('Bool', []), Pos) :-
    Logical =.. [Op, Lhs, Rhs],
    member(Op, [and, or]),
    !,
    infer_type(Env, Lhs, LhsT, Pos),
    infer_type(Env, Rhs, RhsT, Pos),
    typecheck_logical(LhsT, RhsT, Pos),
    !.
infer_type(Env, if(Cond, Then, Else), T, Pos) :-
    infer_type(Env, Cond, CondT, Pos),
    infer_type(Env, Then, ThenT, Pos),
    infer_type(Env, Else, ElseT, Pos),
    typecheck_if(CondT, ThenT, ElseT, T, Pos),
    !.
infer_type(_, list([]), list(_), _) :- !.
infer_type(Env, list([Head | Tail]), list(HeadT), Pos) :-
    infer_type(Env, Head, HeadT, Pos),
    typecheck_list(Env, HeadT, Tail, Pos),
    !.
infer_type(Env, tuple(N, Elements), tuple(N, Types), Pos) :-
    typecheck_tuple(Env, N, Elements, Types, Pos).
infer_type(env(Env, TEnv), let(Var, Type, Val at VPos, Expr at EPos), T, Pos) :-
    infer_type(env(Env, TEnv), Val, ValT, VPos),
    typecheck_let_def(Type, ValT, Pos),
    put_dict(Var, Env, (Val, ValT, VPos), NewEnv),
    infer_type(env(NewEnv, TEnv), Expr, ExprT, EPos),
    typecheck_let_def(T, ExprT, Pos).
infer_type(env(Env, TEnv), lambda(Args, Expr), T, Pos) :-
    construct_lambda_type(Args, LambdaType, Variables, Pos, ReturnType),
    put_dict(Variables, Env, IntermediateEnv),
    infer_type(env(IntermediateEnv, TEnv), Expr, ReturnType, Pos),
    typecheck_function_type(T, LambdaType, Pos).

% TODO:
% pmatching
% adts

typecheck_rec(rec, _, _, _, _, _) :- !, fail.
typecheck_rec(Inferred, Inferred, Env, Var, Val, Pos) :-
    b_set_dict(Var, Env, (Val, Inferred, Pos)).

typecheck_function_type(T, T, _) :- !.
typecheck_function_type(T0, T1, Pos) :-
    print_type_error(
        Pos,
        'type mismatch in function literal, expected ~w, got ~w',
        [type(T0), type(T1)]
    ).

construct_lambda_type([P], A->RetT, [P:(var, A, Pos)], Pos, RetT) :- !.
construct_lambda_type([P | Ps], A->T, [P:(var, A, Pos) | Ts], Pos, RetT) :-
    construct_lambda_type(Ps, T, Ts, Pos, RetT).

typecheck_let_def(T, T, _) :- !.
typecheck_let_def(T1, T0, Pos) :-
    print_type_error(
        Pos,
        'type mismatch in let definition, expected ~w, got ~w',
        [type(T0), type(T1)]
    ).

typecheck_tuple(Env, 1, Element, ElementType, Pos) :-
    infer_type(Env, Element, ElementType, Pos),
    !.
typecheck_tuple(Env, N, (E, Es), (ET, EsT), Pos) :-
    infer_type(Env, E, ET, Pos),
    M is N - 1,
    typecheck_tuple(Env, M, Es, EsT, Pos).

typecheck_list(_, _, [], _) :- !.

typecheck_list(Env, ExpectedType, [H | T], Pos) :-
    infer_type(Env, H, ExpectedType, Pos),
    !,
    typecheck_list(Env, ExpectedType, T, Pos).
typecheck_list(Env, ExpectedType, Tail, Pos) :-
    infer_type(Env, Tail, list(ExpectedType), Pos),
    !.
typecheck_list(Env, ExpectedType, [H | _], Pos) :-
    infer_type(Env, H, HType, Pos),
    print_type_error(
        Pos,
        'type mismatch between list of type ~w and the list element of type ~w',
        [type(ExpectedType), type(HType)]
    ).
typecheck_list(Env, ExpectedType, Tail, Pos) :-
    infer_type(Env, Tail, TailType, Pos),
    print_type_error(
        Pos,
        'type mismatch between list of type ~w and the list tail of type ~w',
        [type(ExpectedType), type(TailType)]
    ).

typecheck_if(adt('Bool', []), T, T, T, _) :- !.
typecheck_if(adt('Bool', []), ThenT, ElseT, _, Start) :-
    !,
    print_type_error(
        Start,
        'type mismatch in if expression.\c
         Type ~w of consequence does not match the type ~w of alternative',
        [type(ThenT), type(ElseT)]
    ).
typecheck_if(CondT, _, _, Start, _) :-
    print_type_error(
        Start,
        'invalid type of conditional expression.\c
         Expected Bool, got ~w',
        [type(CondT)]
    ).

typecheck_logical(adt('Bool', []), adt('Bool', []), _) :- !.
typecheck_logical( Rhs, Lhs, Start) :-
    print_type_error(
        Start,
        'operator ~w type mismatch in logical operator arguments.\c
         Expected Bool and Bool, got ~w and ~w',
        [type(Lhs), type(Rhs)]
    ).

typecheck_application(A->B, A, B, _) :- !.
typecheck_application(FunT, ArgT, _, Start) :-
    print_type_error(
        Start,
        'type mismatch in function application.\c
         Expression of type ~w can\'t be applied to expression of type ~w',
         [type(FunT), type(ArgT)]
    ).

print_type_error(Start, MsgFormat, MsgArgs) :-
    prettify_types(MsgArgs, Prettified),
    print_error_and_halt(Start, MsgFormat, Prettified).

prettify_types([], []) :- !.
prettify_types([type(T) | Ts], [PT | PTs]) :-
    !,
    prettify_type(T, PT),
    prettify_types(Ts, PTs).
prettify_types([Anything | Ts], [Anything | PTs]) :-
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
