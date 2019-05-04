:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).

% interpret_program(EntryPoint) :-
%     parse_file(EntryPoint, AST),
%     process_definitions(AST, Program, Env).

process_definitions(AST, Program, env(Env, TypeEnv)) :-
    empty_env(EmptyEnv),
    add_definitions_to_env(AST, EmptyEnv, EnvWithDefs, ASTNoDefs),
    empty_type_env(EmptyTypeEnv),
    add_typedefs_to_env(
        EnvWithDefs, 
        ASTNoDefs, 
        EmptyTypeEnv,
        Env, 
        Program,
        TypeEnv
    ).