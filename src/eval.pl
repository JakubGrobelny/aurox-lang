:- ensure_loaded(utility).

eval(_-Xs, id(Var), Val) :-
    member(Var:Val, Xs),
    !.
eval(_-LocEnv, id(Var), Val) :-
    is_dict(LocEnv),
    get_dict(Var, LocEnv, Val),
    !.
eval(GlobEnv-_, id(Var), Res) :-
    !,
    eval(GlobEnv, id(Var), Res).
eval(Env, id(Var), Res) :-
    !,
    get_dict(Var, Env, Val),
    eval(Env, Val, Res).
eval(_, Val, Val) :-
    atomic(Val),
    !.
eval(_, bfun(Fun), bfun(Fun)) :- !.
eval(Env, app(F, Arg), Res) :-
    eval(Env, F, bfun(Fun)),
    !,
    eval(Env, Arg, ArgVal),
    Goal =.. [Fun, ArgVal, Res],
    call(Goal).
eval(Env, app(F, Arg), Res) :-
    !,
    eval(Env, Arg, ArgVal),
    eval(Env, F, closure(CParam, CEnv, CExpr)),
    eval(CEnv-[CParam:ArgVal], CExpr, Res).
eval(Env, pmatch(Expr, Cases), Res) :-
    !,
    eval(Env, Expr, ExprVal),
    eval_pattern_matching(Env, ExprVal, Cases, Res).
eval(_, closure(Param, Env, Expr), closure(Param, Env, Expr)) :- !.
eval(Env, Cons/Arg, Cons/ArgVal) :-
    !,
    eval(Env, Arg, ArgVal).
eval(Env, if(Cond, Cons, _), Res) :-
    eval(Env, Cond, true),
    !,
    eval(Env, Cons, Res).
eval(Env, if(_, _, Alt), Res) :-
    eval(Env, Alt, Res),
    !.
eval(Env, or(Lhs, _), true) :-
    eval(Env, Lhs, true),
    !.
eval(Env, or(_, Rhs), Val) :-
    eval(Env, Rhs, Val),
    !.
eval(Env, and(Lhs, _), LhsVal) :-
    eval(Env, Lhs, LhsVal),
    LhsVal = false,
    !.
eval(Env, and(_, Rhs), Val) :-
    eval(Env, Rhs, Val),
    !.
eval(Env, lambda(Param, Expr), closure(Param, Env, Expr)) :- !.
eval(Env, let(Var, Val, Expr), Res) :-
    !,
    eval(Env, Val, ValV),
    eval(Env-[Var:ValV], Expr, Res).
eval(Env, [H | T], [HVal | TVal]) :-
    !,
    eval(Env, H, HVal),
    eval(Env, T, TVal).
eval(Env, (Fst, Snd), (FstVal, SndVal)) :-
    !,
    eval(Env, Fst, FstVal),
    eval(Env, Snd, SndVal).
eval(Env, (Expr;Exprs), Res) :-
    !,
    eval(Env, Expr, _),
    eval(Env, Exprs, Res).
eval(_, Val, Val).

eval_pattern_matching(_, Val, [], _) :-
    !,
    atomic_list_concat(
        ['Non-exhaustive pattern matching! (No match for value', Val, ')!'], 
        Msg
    ),
    throw(runtume_error(Msg)).
eval_pattern_matching(Env, Val, [case(Predicate, Expr) | _], Res) :-
    Goal =.. [Predicate, Val, Substitutions],
    call(Goal),
    !,
    eval(Env-Substitutions, Expr, Res).
eval_pattern_matching(Env, Val, [_ | Ps], Res) :-
    eval_pattern_matching(Env, Val, Ps, Res).