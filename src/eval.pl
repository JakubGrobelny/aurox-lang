:- ensure_loaded(utility).

eval(Env, id(Var), Res) :-
    !,
    get_dict(Var, Env, Val),
    eval(Env, Val, Res).
eval(Env, if(Cond, Cons, _), Res) :-
    eval(Env, Cond, true),
    !,
    eval(Env, Cons, Res).
eval(Env, let(Var, Val, Expr), Res) :-
    !,
    eval(Env, Val, ValV),
    put_dict(Var, Env, ValV, LocEnv),
    eval(LocEnv, Expr, Res).
eval(Env, if(_, _, Alt), Res) :-
    eval(Env, Alt, Res),
    !.
eval(Env, or(Lhs, _), true) :-
    eval(Env, Lhs, true),
    !.
eval(Env, or(_, Rhs), Val) :-
    eval(Env, Rhs, Val),
    !.
eval(Env, and(Lhs, _), false) :-
    eval(Env, Lhs, false),
    !.
eval(Env, and(_, Rhs), Val) :-
    eval(Env, Rhs, Val),
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
    eval(Env, F, closure(ArgVal, LocEnv, Expr)),
    eval(LocEnv, Expr, Res).
eval(Env, lambda(Param, Expr), closure(Arg, LocEnv, Expr)) :-
    !,
    put_dict(Param, Env, Arg, LocEnv).
eval(Env, (Expr;Exprs), Res) :-
    !,
    eval(Env, Expr, _),
    eval(Env, Exprs, Res).
eval(Env, [H | T], [HVal | TVal]) :-
    !,
    eval(Env, H, HVal),
    eval(Env, T, TVal).
eval(Env, (Fst, Snd), (FstVal, SndVal)) :-
    !,
    eval(Env, Fst, FstVal),
    eval(Env, Snd, SndVal).
eval(Env, pmatch(Expr, Cases), Res) :-
    !,
    eval(Env, Expr, ExprVal),
    eval_pattern_matching(Env, ExprVal, Cases, Res).
eval(Env, ADT, Res) :-
    ADT =.. [Constructor, Arg],
    !,
    eval(Env, Arg, ArgVal),
    Res =.. [Constructor, ArgVal].
eval(_, Val, Val).

eval_pattern_matching(_, Val, [], _) :-
    !,
    atomic_list_concat(
        ['Non-exhaustive pattern matching! (No match for value', Val, ')!'], 
        Msg
    ),
    throw(runtume_error(Msg)).
eval_pattern_matching(Env, Val, [case(Pattern, Expr) | _], Res) :-
    replace_pattern_vars(Pattern, Env, NewPattern, LocEnv),
    Val = NewPattern,
    !,
    eval(LocEnv, Expr, Res).
eval_pattern_matching(Env, Val, [_ | Rest], Res) :-
    eval_pattern_matching(Env, Val, Rest, Res).

replace_pattern_vars(id(Var), Env, LogVar, Env) :-
    get_dict(Var, Env, Val),
    var(Val),
    !,
    LogVar = Val.
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
replace_pattern_vars(ADT, Env, NewADT, NewEnv) :-
    ADT =.. [Constructor, Arg],
    !,
    % format('DEBUG: ~w\n', [Arg]),
    replace_pattern_vars(Arg, Env, NewArg, NewEnv),
    NewADT =.. [Constructor, NewArg].
replace_pattern_vars(X, Env, X, Env).
