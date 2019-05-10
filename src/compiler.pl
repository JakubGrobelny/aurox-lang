:- ensure_loaded(utility).

compile_env(Env, Stream) :-
    write_boilerplate(Stream),
    dict_pairs(Env, _, Contents),
    write_function_prototypes(Contents, Stream),
    compile_env_helper(Contents, Stream).

compile_program(Program, Stream) :-
    format(Stream, "int main(int argc, char* argv[])\n{\n", []),
    compile_program_helper(Program, Stream),
    format(Stream, "return 0;\n}\n", []).

compile_program([], _) :- !.
compile_program([Expr at _ | Exprs], File) :-
    compile_expr(Expr, File),
    compile_program(Exprs, File).

write_function_prototypes([], _) :- !.
write_function_prototypes(['`types'-_ | Env], File) :-
    !,
    compile_env_helper(Env, File).
% TODO: case for bfun (maybe)
write_function_prototypes([Var-(lambda(Arg, _, _), (A->B), _) | Env], File) :-
    !,
    write_function_signature(A, B, Arg, Var, File),
    format(File, ";", []),
    write_function_prototypes(Env, File).
write_function_prototypes([_ | Env], File) :-
    write_function_prototypes(Env, File).

write_function_signature(ArgT, RetT, ArgName, FunName, File) :-
    translate_type(ArgT, ArgCppType),
    translate_type(RetT, RetCppType),
    normalize_name(FunName, CppName),
    normalize_name(ArgName, CppArg),
    format(
        File, 
        "~w ~w(const ~w ~w);", 
        [RetCppType, CppName, ArgCppType, CppArg]
    ).

compile_env_helper([], _) :- !.
compile_env_helper(['`types'-_ | Env], File) :-
    !,
    compile_env_helper(Env, File).
compile_env_helper([Var-(lambda(Arg, Expr) at _, (A->B), _) | Env], File) :-
    write_function_signature(A, B, Arg, Var, File),
    format(File, "{", []),
    compile_expr(Expr, File),
    format(File,"\n}\n", []),
    compile_env_helper(Env, File).
compile_env_helper([Var-(Val at _, Type, _) | Env], File) :-
    translate_type(Type, CppType),
    normalize_name(Var, CppName),
    format(File, "~w ~w =", [CppType, CppName]),
    compile_expr(Val, File),
    format(File, "\n", []),
    compile_env_helper(Env, File).










