:- ensure_loaded(utility).
:- ensure_loaded(typechecker).

% TODO:
% 1. add type information and env during compilation

compile_env(Env, Stream) :-
    write_boilerplate(Stream),
    dict_pairs(Env, _, Contents),
    write_function_prototypes(Contents, Stream),
    compile_env_helper(Contents, Env, Stream).

compile_program(Program, Env, Stream) :-
    format(Stream, "int main(int argc, char* argv[])\n{\n", []),
    compile_program_helper(Program, Env, Stream),
    format(Stream, "return 0;\n}\n", []).

compile_program_helper([], Env, _) :- !.
compile_program_helper([Expr at _ | Exprs], Env, File) :-
    compile_expr(Expr, Env, File),
    compile_program_helper(Exprs, Env, File).

write_boilerplate(File) :-
    fix_file_path('boilerplate.cpp', BoilerplatePath),
    open(BoilerplatePath, read, Boilerplate),
    format(Boilerplate, "//file generated automatically\n\n", []),
    read_string(Boilerplate, "", "", _, Contents),
    format(File, Contents, []),
    close(Boilerplate).

compile_expr(int(N), _, File) :-
    !,
    format(File, " ~w ", [N]).
compile_expr(float(X), _, File) :-
    !,
    format(File, " ~w ", [X]).
compile_expr(char(C), _, File) :-
    !,
    % TODO: escape sequences
    format(File, " '~w' ", [C]).
compile_expr(bool(B), _, File) :-
    !,
    format(File, " ~w ", [B]).
compile_expr(unit, _, File) :-
    !,
    format(File, " unit::unit ", []).
compile_expr(wildcard, _, File) :-
    !,
    format(File," __wildcard ", []).
compile_expr(id(Var), _, File) :-
    !,
    normalize_name(Var, NVar),
    format(File, " __~w__ ", [NVar]).
compile_expr(enum(Name), _, File) :-
    !,
    normalize_name(Name, NName),
    format(File, " __enum(\"~w\") ", [NName]).
compile_expr(adt(Name, Arg), Env, File) :-
    !,
    infer_type(Env, adt(Name, Arg), adt(_, ParamType), _),
    translate_type(ParamType, CppType),
    format(File,  "__constructor<~w>(\"~w\",", [Name, CppType]),
    compile_expr(Arg, Env, File),
    format(File, ")\n", []).
compile_expr(if(Cond at _, Cons at _, Alt at _), Env, File) :-
    !,
    format(File, "if (", []),
    compile_expr(Cond, Env, File),
    format(File, ") {\n", []),
    compile_expr(Cons, Env, File),
    format(File, "\n} else {\n", []),
    compile_expr(Alt, Env, File),
    format(File, "\n}\n", []).
compile_expr(let((id(Var), Type, Val at _, Expr at _), Env, File) :-
    !,
    format(File, "(", []),
    compile_expr(lambda(Var, Expr), Env, File),


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
    translate_type(ArgT, ArgCppType, ArgVars),
    translate_type(RetT, RetCppType, RetVars),
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
    translate_type(Type, CppType, _),
    normalize_name(Var, CppName),
    format(File, "~w ~w =", [CppType, CppName]),
    compile_expr(Val, File),
    format(File, "\n", []),
    compile_env_helper(Env, File).

translate_type(Var, Type) :-
    var(Var),
    !,
    term_string(Var, StrVar),
    string_concat("V", StrVar, Type).
translate_type(param(A), Type) :-
    !,
    atom_string(A, AStr),
    string_upper(AStr, Type).
translate_type(adt(Name, []), Type) :-
    member(Name, ['Int', 'Bool', 'Float', 'Char', 'Unit']),
    !,
    get_dict(
        Name,
        types{
            'Int'   :"int64_t", 
            'Bool'  :"bool",
            'Float' :"double", 
            'Char'  :"char",
            'Unit'  :"unit"
        }, 
        Type
    ).
translate_type(adt(_, A), Type) :-
    !,
    translate_type(A, AType),
    sformat(Type, "__constructor<~w>", [AType]).
translate_type(list(Xs), Type) :-
    !,
    translate_type(Xs, XsType),
    sformat(Type, "__list<~w>", [XsType]).
translate_type(tuple(_, Elements), Type) :-
    !,
    translate_tuple_types(Elements, TypesList),
    atomic_list_concat(TypesList, ', ', TupleType),
    sformat(Type, "std::tuple<~w>", [TupleType]).
translate_type((A->B), Type) :-
    !,
    translate_type(A, AT),
    translate_type(B, BT),
    sformat(Type, "std::function<~w(~w)>", [BT, AT]).

translate_tuple_types((X,Xs), [T|Ts]) :-
    !,
    translate_type(X, T),
    translate_tuple_types(Xs, Ts).
translate_tuple_types(X, [T]) :-
    translate_type(X, T).

normalize_name(Identifier, Normalized) :-
    atom_chars(Identifier, Chars),
    normalize_characters(Chars, NormalizedChars),
    atomic_list_concat(NormalizedChars, Normalized).

normalize_characters([C|Cs], [NC | NCs]) :-
    !,
    normalize_char(C, NC),
    normalize_characters(Cs, NCs).
normalize_characters([], []).

normalize_char(C, C) :-
    is_digit(C),
    !.
normalize_char(C, C) :-
    char_code(C, Code),
    Code >= 65,
    Code =< 90,
    !.
normalize_char(C, C) :-
    char_code(C, Code),
    Code >= 97,
    Code =< 122,
    !.
normalize_char(C, NC) :-
    char_code(C, Code),
    term_to_atom(Code, Atom),
    atomic_list_concat(['_', Atom], NC).

