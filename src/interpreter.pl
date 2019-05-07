:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).
:- ensure_loaded(pervasives).

interpret_program(EntryPoint) :-
    parse_file(EntryPoint, AST),
    import_core_definitions(FreshEnv),
    process_definitions(AST, FreshEnv,_, Env),
    typecheck_environment(Env),
    pretty_env(Env).

process_definitions(AST, EnvIn, Program, EnvOut) :-
    add_definitions_to_env(AST, EnvIn, EnvWithDefs, ASTNoDefs),
    add_typedefs_to_env(ASTNoDefs, EnvWithDefs, EnvOut, Program).
