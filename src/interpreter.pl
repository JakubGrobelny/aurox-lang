:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).

interpret_program(EntryPoint) :-
    parse_file(EntryPoint, AST),
    process_definitions(AST, _, Env),
    typecheck_environment(Env),
    pretty_env(Env).
 
process_definitions(AST, Program, Env) :-
    empty_env(EmptyEnv),
    add_definitions_to_env(AST, EmptyEnv, EnvWithDefs, ASTNoDefs),
    add_typedefs_to_env(ASTNoDefs, EnvWithDefs, Env, Program).