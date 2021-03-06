:- ensure_loaded(utility).

preprocess_program([], []) :- !.
preprocess_program([Expr at _ | Exprs], [NewExpr | NewExprs]) :-
    preprocess_expr(Expr, NewExpr),
    preprocess_program(Exprs, NewExprs).

preprocess_env(Env, NewEnv, Contents) :-
    dict_pairs(Env, _, Contents),
    preprocess_env_helper(Contents, PreprocessedContents),
    dict_create(NewEnv, globenv, PreprocessedContents).

preprocess_env_helper([], []) :- !.
preprocess_env_helper(['`types'-_ | Env], PEnv) :-
    !,
    preprocess_env_helper(Env, PEnv).
preprocess_env_helper([Var-(Val at _, _, _) | Env], [(Var:NewVal) | NewEnv]) :-
    preprocess_expr(Val, NewVal),
    preprocess_env_helper(Env, NewEnv).

preprocess_expr(pmatch(Expr at _, Cases), pmatch(PExpr, PCases)) :-
    !,
    preprocess_expr(Expr, PExpr),
    preprocess_pmatch_cases(Cases, PCases).
preprocess_expr(and(Lhs, Rhs), and(NLhs, NRhs)) :-
    !,
    preprocess_expr(Lhs, NLhs),
    preprocess_expr(Rhs, NRhs).
preprocess_expr(or(Lhs, Rhs), or(NLhs, NRhs)) :-
    !,
    preprocess_expr(Lhs, NLhs),
    preprocess_expr(Rhs, NRhs).
preprocess_expr(let(id(Var), _, Val at _, Expr at _), let(Var, NVal, NExpr)) :-
    !,
    preprocess_expr(Val, NVal),
    preprocess_expr(Expr, NExpr).
preprocess_expr(lambda(Arg, Expr), lambda(Arg, PExpr)) :-
    !,
    preprocess_expr(Expr, PExpr).
preprocess_expr(if(Cond at _, Cons at _, Alt at _), if(PCond, PCons, PAlt)) :-
    !,
    preprocess_expr(Cond, PCond),
    preprocess_expr(Cons, PCons),
    preprocess_expr(Alt, PAlt).
preprocess_expr(bfun(F), bfun(F)) :- !.
preprocess_expr(app(F, A), app(PF, PA)) :-
    !,
    preprocess_expr(F, PF),
    preprocess_expr(A, PA).
preprocess_expr(int(N), N) :- !.
preprocess_expr(float(X), X) :- !.
preprocess_expr(char(C), C) :- !.
preprocess_expr(bool(B), B) :- !.
preprocess_expr(unit, unit) :- !.
preprocess_expr(wildcard, wildcard) :- !.
preprocess_expr(id(Var), id(Var)) :- !.
preprocess_expr(enum(Name), Name) :- !.
preprocess_expr(adt(Name, Arg), Name/NewArg) :-
    !,
    preprocess_expr(Arg, NewArg).
preprocess_expr(Sequence, PreprocessedSequence) :-
    is_list(Sequence),
    !,
    preprocess_sequence(Sequence, PreprocessedSequence).
preprocess_expr(list(Xs), Ys) :-
    !,
    preprocess_list(Xs, Ys).
preprocess_expr(tuple(_, Elements), PreprocessedElements) :-
    preprocess_tuple(Elements, PreprocessedElements).
preprocess_tuple((Expr, Exprs), (PPExpr, PPExprs)) :-
    !,
    preprocess_expr(Expr, PPExpr),
    preprocess_tuple(Exprs, PPExprs).
preprocess_tuple(Expr, PPExpr) :-
    preprocess_expr(Expr, PPExpr).

preprocess_list([], []) :- !.
preprocess_list([X | Xs], [Y | Ys]) :-
    !,
    preprocess_expr(X, Y),
    preprocess_list(Xs,  Ys).
preprocess_list(Tail, PTail) :-
    preprocess_expr(Tail, PTail).

preprocess_sequence([Expr], PPExpr) :-
    !,
    preprocess_expr(Expr, PPExpr).
preprocess_sequence([E | Es], (PE;PEs)) :-
    preprocess_expr(E, PE),
    preprocess_sequence(Es, PEs).

preprocess_pmatch_cases([], []) :- !.
preprocess_pmatch_cases(
    [case(P, E at _) | Cases], 
    [case(PPAtom, PE) | PCases]
) :-
    preprocess_expr(P, PP),
    replace_pattern_vars(PP, locenv{}, FinalPP, LocEnv),
    dict_pairs(LocEnv, _, Pairs),
    replace_functor(Pairs, ':', Substitutions),
    term_to_atom(FinalPP, PPAtom),
    Goal =.. [PPAtom, FinalPP, Substitutions],
    assert(Goal),
    preprocess_expr(E, PE),
    preprocess_pmatch_cases(Cases, PCases).

replace_pattern_vars(id(Var), Env, LogVar, Env) :-
    get_dict(Var, Env, Val),
    var(Val),
    !,
    LogVar = Val.
replace_pattern_vars(Cons/Arg, Env, Cons/NewArg, NewEnv) :-
    !,
    replace_pattern_vars(Arg, Env, NewArg, NewEnv).
replace_pattern_vars(wildcard, Env, _, Env) :- !.
replace_pattern_vars(id(Var), Env, LogVar, NewEnv) :-
    put_dict(Var, Env, LogVar, NewEnv),
    !.
replace_pattern_vars([], Env, [], Env) :- !.
replace_pattern_vars([X | Xs], Env, [Y | Ys], FinalEnv) :-
    !,
    replace_pattern_vars(X, Env, Y, NewEnv),
    replace_pattern_vars(Xs, NewEnv, Ys, FinalEnv).
replace_pattern_vars((H, T), Env, (NH, NT), FinalEnv) :-
    !,
    replace_pattern_vars(H, Env, NH, NewEnv),
    replace_pattern_vars(T, NewEnv, NT, FinalEnv).
replace_pattern_vars(X, Env, X, Env).



