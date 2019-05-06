:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).


interpret_program(EntryPoint) :-
    parse_file(EntryPoint, AST),
    process_definitions(AST, Program, Env),
    typecheck_environment(Env),
    clean_defs(Program).
 
process_definitions(AST, Program, Env) :-
    empty_env(EmptyEnv),
    add_definitions_to_env(AST, EmptyEnv, EnvWithDefs, ASTNoDefs),
    add_typedefs_to_env(ASTNoDefs, EnvWithDefs, Env, Program).