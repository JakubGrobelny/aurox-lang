:- ensure_loaded(lexer).
:- ensure_loaded(parser).
:- ensure_loaded(interpreter).
:- ensure_loaded(typechecker).

repl :-
    import_core_definitions(FreshEnv),
    import_core_module(FreshEnv, NewEnv, Operators),
    repl(NewEnv, Operators, []).

repl(Env, Ops, Deps) :-
    catch(
        try_repl(Env, Ops, Deps),
        _,
        repl(Env, Ops, Deps)
    ).
    
try_repl(Env, Ops, Deps) :-
    read_input(Input),
    repl_eval(Input, Env, Ops, Deps, NewEnv, NewOps, NewDeps),
    try_repl(NewEnv, NewOps, NewDeps).

read_input(Input) :-
    read_input([], Input).
read_input(Acc, Res) :-
    read_line_to_codes(user_input, Line),
    char_codes_to_atoms(Line, LineAtoms),
    \+ Line = [],
    !,
    append(Acc, ['\n' | LineAtoms], NewAcc),
    read_input(NewAcc, Res).
read_input(Acc, Acc).

repl_eval(Input, Env, Ops, Deps, ProcessedEnv, NewOps, NewDeps) :-
    parse_input(Input, Deps, Ops, NewDeps, NewOps, AST),
    process_definitions(AST, Env, ProcessedAST, ProcessedEnv),
    copy_term(ProcessedEnv, ProcessedEnvCopy),
    typecheck_environment(ProcessedEnvCopy),
    typecheck_repl_program(ProcessedEnvCopy, ProcessedAST, Types),
    preprocess_env(ProcessedEnvCopy, PreprocessedEnv, Contents),
    eval_env(PreprocessedEnv, Contents, EvaluatedEnv),
    preprocess_program(ProcessedAST, PreprocessedAST),
    time(run_repl_program(PreprocessedAST, EvaluatedEnv, Types)).

run_repl_program([], _, []) :- !.
run_repl_program([Expr | Exprs], Env, [Type | Types]) :-
    eval(Env, Expr, Val),
    prettify_type(Type, 97, _, PType),
    prettify_expr(Val, PVal),
    format("~w :: ~w\n", [PVal, PType]),
    run_repl_program(Exprs, Env, Types).
    
typecheck_repl_program(_, [], []) :- !.
typecheck_repl_program(Env, [Expr at Pos | Exprs], [Type | Types]) :-
    infer_type(Env, Expr, Type, Pos),
    !,
    typecheck_repl_program(Env, Exprs, Types).
typecheck_repl_program(_, [_ at Pos | _], _) :-
    print_error_and_halt(
        Pos,
        'type of expression couldn\'t have been inferred',
        []
    ).

tokenize_input(Input, Tokens) :-
    catch(
        phrase(lexer(Tokens, pos(stdin, 1, 1)), Input),
        error(Format, Args) at Pos,
        print_error_and_halt(Pos, Format, Args)
    ).

parse_input(Input, Deps, Ops, NewDeps, NewOps, AST) :-
    tokenize_input(Input, Tokens),
    catch(
        phrase(
            program(
                AST,
                Ops,
                NewOps,
                Deps,
                NewDeps
            ),
            Tokens
        ),
        error(Format, Args) at Pos,
        print_error_and_halt(Pos, Format, Args)
    ).



