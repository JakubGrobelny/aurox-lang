:- ensure_loaded(parser).
:- ensure_loaded(typechecker).
:- ensure_loaded(environment).

interpret_program(EntryPoint) :-
    parse_file(EntryPoint, AST),
    empty_environment(EmptyEnv),
    add_definitions_to_environment(EmptyEnv, AST, Env),
    typecheck_environment(Env).