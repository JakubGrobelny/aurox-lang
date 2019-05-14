:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).
:- ensure_loaded(intrinsics).
:- ensure_loaded(preprocessing).
:- ensure_loaded(repl).
:- ensure_loaded(eval).

interpret_program(EntryPoint) :-
    import_core_definitions(FreshEnv),
    import_core_module(FreshEnv, NewEnv, Operators), 
    parse_file(EntryPoint, Operators, AST),
    process_definitions(AST, NewEnv, Program, FinalEnv),
    typecheck_environment(FinalEnv),
    typecheck_program(FinalEnv, Program),
    preprocess_env(FinalEnv, PreprocessedEnv, Contents),
    eval_env(PreprocessedEnv, Contents, EvaluatedEnv),
    preprocess_program(Program, PreprocessedProgram),
    time(run_program(PreprocessedProgram, EvaluatedEnv, _)).

eval_env(Env, [], Env) :- !.
eval_env(Env, ['`types'-_ | Vars], NewEnv) :-
    !,
    eval_env(Env, Vars, NewEnv).
eval_env(Env, [Var-_ | Vars], FinalEnv) :-
    eval(Env, id(Var), Val),
    put_dict(Var, Env, Val, NewEnv),
    eval_env(NewEnv, Vars, FinalEnv).

run_program([], _, []) :- !.
run_program([Expr | Exprs], Env, [Val | Vals]) :-
    eval(Env, Expr, Val),
    run_program(Exprs, Env, Vals).

process_definitions(AST, EnvIn, Program, EnvOut) :-
    add_definitions_to_env(AST, EnvIn, EnvWithDefs, ASTNoDefs),
    add_typedefs_to_env(ASTNoDefs, EnvWithDefs, EnvOut, Program).

import_core_module(Env, NewEnv, Operators) :-
    fix_file_path('modules/Core.ax', File),
    parse_file(File, Core, pos('Core',0,0), [], _, Operators),
    process_definitions(Core, Env, [], NewEnv).
