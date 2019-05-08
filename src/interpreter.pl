:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).
:- ensure_loaded(pervasives).

interpret_program(EntryPoint, Program) :-
    import_core_definitions(FreshEnv),
    import_core_module(FreshEnv, NewEnv, Operators),
    parse_file(EntryPoint, Operators, AST),
    process_definitions(AST, NewEnv, Program, FinalEnv),
    typecheck_environment(FinalEnv),
    typecheck_program(FinalEnv, Program).

process_definitions(AST, EnvIn, Program, EnvOut) :-
    add_definitions_to_env(AST, EnvIn, EnvWithDefs, ASTNoDefs),
    add_typedefs_to_env(ASTNoDefs, EnvWithDefs, EnvOut, Program).

import_core_module(Env, NewEnv, Operators) :-
    fix_file_path('modules/Core.ax', File),
    parse_file(
        File, 
        Core, 
        pos('Core',0,0), 
        [], 
        _, 
        Operators
    ),
    process_definitions(Core, Env, [], NewEnv).
