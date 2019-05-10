:- ensure_loaded(utility).
:- ensure_loaded(environment).

typecheck_program(_, []) :- !.
typecheck_program(Env, [Expr at Pos | Exprs]) :-
    infer_type(Env, Expr, _, Pos),
    !,
    typecheck_program(Env, Exprs).
typecheck_program(_, [_ at Pos | _]) :-
    print_error_and_halt(
        Pos,
        'type of expression couldn\'t have been inferred',
        []
    ).

typecheck_environment(Env) :-
    dict_pairs(Env, _, Contents),
    typecheck_environment(Contents, Env).

typecheck_environment([], _) :- !.
typecheck_environment([('`types'-_) | Tail], Env) :-
    !,
    typecheck_environment(Tail, Env).
typecheck_environment([_-(_, _, builtin) | Tail], Env) :-
    !,
    typecheck_environment(Tail, Env).
typecheck_environment([_-(Val at Pos, TSig, _) | Vars], Env) :-
    get_dict('`types', Env, Types),
    check_if_types_defined(Types, TSig, Pos),
    fix_type_signature(TSig, Type),
    infer_type(Env, Val, TSig, Pos),
    !,
    \+ var(Type),
    typecheck_environment(Vars, Env).
typecheck_environment([Var-(Val at ValPos, Type, Pos) | _], Env) :-
    infer_type(Env, Val, ValType, Pos),
    \+ var(ValType),
    !,
    print_type_error(
        ValPos,
        'The type ~w of variable ~w does not match the type \c 
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

fix_type_signature(Type, FixedType) :-
    extract_variables_from_type(Type, Vars),
    sort(Vars, VarsSorted),
    params_to_keys(VarsSorted, Keys),
    dict_create(Map, map, Keys),
    map_variable_to_type(Type, Map, FixedType).

infer_type(Env, id(Var), Type, _) :-
    get_dict(Var, Env, (_, Type, builtin)),
    !.
infer_type(Env, id(Var), Type, _) :-
    get_dict(Var, Env, (var, PrevType, Pos)),
    \+ var(PrevType),
    PrevType = rec,
    !,
    b_set_dict(Var, Env, (var, Type, Pos)).
infer_type(Env, id(Var), Type, _) :-
    get_dict(Var, Env, (var, Type, _)),
    !.
infer_type(Env, id(Var), Type, _) :-
    get_dict(Var, Env, (Val, PrevType, DefPos)),
    \+ var(PrevType),
    PrevType = rec,
    !,
    b_set_dict(Var, Env, (Val, Type, DefPos)).
infer_type(Env, id(Var), ExpectedType, _) :-
    get_dict(Var, Env, (_, Type, _)),
    \+ var(Type),
    !,
    copy_term(Type, Copy),
    fix_type_signature(Copy, ExpectedType).
infer_type(Env, id(Var), ExpectedType, _) :-
    get_dict(Var, Env, (Val at Pos, Type, DefPos)),
    b_set_dict(Var, Env, (Val at Pos, rec, DefPos)),
    !,
    infer_type(Env, Val, InferredType, DefPos),
    typecheck_rec(InferredType, Type, Env, Var, Val at Pos, DefPos),
    copy_term(Type, Copy),
    fix_type_signature(Copy, ExpectedType).
infer_type(_, id(Var), _, Pos) :-
    print_error_and_halt(
        Pos,
        'undefined identifier ~w',
        [Var]
    ).
infer_type(Env, [Expr], Type, Pos) :-
    !,
    infer_type(Env, Expr, Type, Pos).
infer_type(Env, [H | T], Type, Pos) :-
    !,
    infer_type(Env, H, _, Pos),
    infer_type(Env, T, Type, Pos).
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
infer_type(Env, if(Cond at CPos, Then at TPos, Else at EPos), T, Pos) :-
    infer_type(Env, Cond, CondT, CPos),
    infer_type(Env, Then, ThenT, TPos),
    infer_type(Env, Else, ElseT, EPos),
    typecheck_if(CondT, ThenT, ElseT, T, Pos),
    !.
infer_type(_, list([]), list(_), _) :- !.
infer_type(Env, list([Head | Tail]), list(HeadT), Pos) :-
    infer_type(Env, Head, HeadT, Pos),
    typecheck_list(Env, HeadT, Tail, Pos),
    !.
infer_type(Env, tuple(N, Elements), tuple(N, Types), Pos) :-
    typecheck_tuple(Env, N, Elements, Types, Pos).
infer_type(Env, let(id(Var), Type, Val at VPos, Expr at EPos), T, Pos) :-
    infer_type(Env, Val, ValT, VPos),
    typecheck_let_def(Type, ValT, Pos),
    put_dict(Var, Env, (Val, ValT, VPos), NewEnv),
    infer_type(NewEnv, Expr, ExprT, EPos),
    typecheck_let_def(T, ExprT, Pos).
infer_type(Env, lambda(Arg, Expr), T, Pos) :-
    construct_lambda_type(Arg, LambdaType, Variables, Pos, ReturnType),
    put_dict(Variables, Env, IntermediateEnv),
    infer_type(IntermediateEnv, Expr, ReturnType, Pos),
    typecheck_function_type(T, LambdaType, Pos).
infer_type(_, wildcard, _, _) :- !.
infer_type(Env, adt(Cons, Arg), TCopy, Pos) :-
    infer_type(Env, Arg, ArgType, Pos),
    get_dict(Cons, Env, (_, (A->T), _)),
    copy_term((A->T), (ACopy->TCopy)),
    typecheck_adt_argument(ArgType, ACopy, Pos),
    !.
infer_type(Env, enum(Cons), TCopy, _) :-
    get_dict(Cons, Env, (_, T, _)),
    copy_term(T, TCopy),
    !.
infer_type(Env, pmatch(Expr at EPos, Patterns), T, Pos) :-
    infer_type(Env, Expr, ExprT, EPos),
    typcheck_pmatching(Env, Patterns, ExprT, T, Pos).

typecheck_adt_argument(T, T, _) :- !.
typecheck_adt_argument(Got, Expected, Pos) :-
    print_type_error(
        Pos,
        'constructor argument type mismatch, expected ~w, got ~w',
        [type(Expected), type(Got)]
    ).

typcheck_pmatching(_, [], adt('Void', []), _, _) :- !.
typcheck_pmatching(_, [], Type, _, Pos) :-
    print_type_error(
        Pos,
        'empty pattern matching, expected Void, got ~w',
        [type(Type)]
    ).
typcheck_pmatching(Env, [case(P, E at CPos)], ExpectedType, T, _) :-
    !,
    extract_pattern_variables(P, Env, NewEnv, CPos),
    infer_type(NewEnv, P, PT, CPos),
    pattern_type_matches(ExpectedType, PT, CPos),
    infer_type(NewEnv, E, ET, CPos),
    typecheck_pmatching_exprs(ET, T, CPos).
typcheck_pmatching(Env, [case(P, E at CPos)| Ps], ExpectedType, T, Pos) :-
    extract_pattern_variables(P, Env, NewEnv, CPos),
    infer_type(NewEnv, P, PT, CPos),
    pattern_type_matches(ExpectedType, PT, CPos),
    infer_type(NewEnv, E, ET, CPos),
    typecheck_pmatching_exprs(T, ET, CPos),
    typcheck_pmatching(Env, Ps, ExpectedType, T, Pos).

extract_pattern_variables(wildcard, Map, Map, _) :- !.
extract_pattern_variables(id(A), Map, NewMap, Pos) :-
    put_dict(A, Map, (var, _, Pos), NewMap),
    !.
extract_pattern_variables(adt(_, Pattern), Map, NewMap, Pos) :-
    !,
    extract_pattern_variables(Pattern, Map, NewMap, Pos).
extract_pattern_variables(tuple(_, Elems), Map, NewMap, Pos) :-
    !,
    list_of_tuple(Elems, List),
    pattern_variables_from_list(List, Map, NewMap, Pos).
extract_pattern_variables(list(Xs), Map, NewMap, Pos) :-
    !,
    pattern_variables_from_list(Xs, Map, NewMap, Pos).
extract_pattern_variables(_, Map, Map, _).

pattern_variables_from_list([], Map, Map, _) :- !.
pattern_variables_from_list([H | T], Map, FinalMap, Pos) :-
    !,
    extract_pattern_variables(H, Map, NewMap, Pos),
    pattern_variables_from_list(T, NewMap, FinalMap, Pos).
pattern_variables_from_list(Tail, Map, NewMap, Pos) :-
    extract_pattern_variables(Tail, Map, NewMap, Pos).

pattern_type_matches(T, T, _) :- !.
pattern_type_matches(ExpectedT, T, Pos) :-
    print_type_error(
        Pos,
        'pattern type mismatch, expected ~w, got ~w',
        [type(ExpectedT), type(T)]
    ).

typecheck_pmatching_exprs(T, T, _) :- !.
typecheck_pmatching_exprs(T0, T1, CPos) :-
    print_type_error(
        CPos,
        'pattern case expression type mismatch, expected ~w, got ~w',
        [type(T0), type(T1)]
    ).

typecheck_rec(rec, _, _, _, _, _) :- !, fail.
typecheck_rec(Inferred, Inferred, Env, Var, Val, Pos) :-
    b_set_dict(Var, Env, (Val, Inferred, Pos)).

typecheck_function_type(T, T, _) :- !.
typecheck_function_type(T0, T1, Pos) :-
    print_type_error(
        Pos,
        'type mismatch in function, expected ~w, got ~w',
        [type(T0), type(T1)]
    ).

construct_lambda_type(P, A->RetT, [P:(var, A, Pos)], Pos, RetT).

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
typecheck_logical(Rhs, Lhs, Start) :-
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
        'type mismatch in function application. \c
         Expression of type ~w can\'t be applied to expression of type ~w',
         [type(FunT), type(ArgT)]
    ).

print_type_error(Start, MsgFormat, MsgArgs) :-
    prettify_types(MsgArgs, Prettified),
    print_error_and_halt(Start, MsgFormat, Prettified).

prettify_types(Types, Prettified) :-
    char_code(a, Code),
    prettify_types(Types, Code, _, Prettified).

prettify_types([], Code, Code, []) :- !.
prettify_types([type(T) | Ts], Code, FinalCode, [PT | PTs]) :-
    !,
    prettify_type(T, Code, NewCode, PT),
    prettify_types(Ts, NewCode, FinalCode, PTs).
prettify_types([X | Ts], Code, NewCode, [X | PTs]) :-
    prettify_types(Ts, Code, NewCode, PTs).

prettify_type_list([], Code, Code, []) :- !.
prettify_type_list([T | Ts], Code, FinalCode, [TAtom | TsAtoms]) :-
    prettify_type(T, Code, NewCode, TAtom),
    prettify_type_list(Ts, NewCode, FinalCode, TsAtoms).

prettify_type(X, Code, NewCode, Atom) :-
    var(X),
    !,
    char_code(Atom, Code),
    NewCode is Code + 1.
prettify_type(adt(Name, Params), Code, NewCode, Atom) :-
    !,
    prettify_type_list(Params, Code, NewCode, PParams),
    atomic_list_concat([Name | PParams], ' ', Atom).
prettify_type(list(Type), Code, NewCode, Atom) :-
    !,
    prettify_type(Type, Code, NewCode, TypeAtom),
    atomic_list_concat(['[', TypeAtom, ']'], Atom).
prettify_type(tuple(_, Elements), Code, NewCode, Atom) :-
    !,
    list_of_tuple(Elements, ElementsList),
    prettify_type_list(ElementsList, Code, NewCode, Prettified),
    atomic_list_concat(Prettified, ', ', AtomWithoutBrackets),
    atomic_list_concat(['(', AtomWithoutBrackets, ')'], Atom).
prettify_type(param(A), Code, Code, A) :- !.
prettify_type((T0->T1), Code, FinalCode, Atom) :-
    !,
    prettify_type(T0, Code, NewCode, T0Atom),
    prettify_type(T1, NewCode, FinalCode, T1Atom),
    atomic_list_concat(['(', T0Atom, '->', T1Atom, ')'], Atom).
prettify_type(X, Code, Code, X).

check_if_types_defined(_, Var, _) :-
    var(Var),
    !.
check_if_types_defined(_, param(_), _) :- !.
check_if_types_defined(_, none, _) :- !.
check_if_types_defined(Types, adt(Name, Params), _) :-
    get_dict(Name, Types, (N, _)),
    length(Params, N),
    !.
check_if_types_defined(_, adt(Name, Params), Pos) :-
    !,
    print_type_error(
        Pos,
        'undefined type ~w',
        [type(adt(Name, Params))]
    ).
check_if_types_defined(Types, [T | Ts], Pos) :-
    !,
    check_if_types_defined(Types, T, Pos),
    check_if_types_defined(Types, Ts, Pos).
check_if_types_defined(_, [], _) :- !.
check_if_types_defined(Types, tuple(_, Ts), Pos) :-
    !,
    list_of_tuple(Ts, TsList),
    check_if_types_defined(Types, TsList, Pos).
check_if_types_defined(Types, T, Pos) :-
    T =.. [_ | Args],
    check_if_types_defined(Types, Args, Pos).

extract_variables_from_type(Var, []) :-
    var(Var),
    !.
extract_variables_from_type(none, []) :- !.
extract_variables_from_type(param(A), [A]) :- !.
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
    list_of_tuple(Types, List),
    extract_variables_from_type(List, Params).
extract_variables_from_type(T, Params) :-
    T =.. [_ | Args],
    extract_variables_from_type(Args, Params).

map_variable_to_type(Var, _, Var) :-
    var(Var),
    !.
map_variable_to_type(none, _, none) :- !.
map_variable_to_type(param(A), Map, Var) :-
    get_dict(A, Map, Var),
    !.
map_variable_to_type(adt(N, Params), Map, adt(N, MappedParams)) :-
    !,
    map_variable_to_type(Params, Map, MappedParams).
map_variable_to_type([T | Types], Map, [MT | MTypes]) :-
    !,
    map_variable_to_type(T, Map, MT),
    map_variable_to_type(Types, Map, MTypes).
map_variable_to_type([], _, []) :- !.
map_variable_to_type(tuple(N, Types), Map, tuple(N, MTypes)) :-
    !,
    map_variable_to_tuple(Types, Map, MTypes).
map_variable_to_type(T, Map, MT) :-
    T =.. [Op | Args],
    map_variable_to_tuple(Args, Map, MArgs),
    MT =.. [Op | MArgs].
    
map_variable_to_tuple((H, T), Map, (MH, MT)) :-
    !,
    map_variable_to_type(H, Map, MH),
    map_variable_to_tuple(T, Map, MT).
map_variable_to_tuple(T, Map, MT) :-
    map_variable_to_type(T, Map, MT).

