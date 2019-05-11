:- ensure_loaded(utility).

% TODO:
% 1. add type information and env during compilation


compile_env(Env, Stream) :-
    write_boilerplate(Stream),
    dict_pairs(Env, _, Contents),
    write_function_prototypes(Contents, Stream),
    compile_env_helper(Contents, Stream).

compile_program(Program, Stream) :-
    format(Stream, "int main(int argc, char* argv[])\n{\n", []),
    compile_program_helper(Program, Stream),
    format(Stream, "return 0;\n}\n", []).

compile_program_helper([], _) :- !.
compile_program_helper([Expr at _ | Exprs], File) :-
    compile_expr(Expr, File),
    compile_program(Exprs, File).

write_boilerplate(File) :-
    fix_file_path('boilerplate.cpp', BoilerplatePath),
    open(BoilerplatePath, read, Boilerplate),
    read_string(Boilerplate, "", "", _, Contents),
    format(File, Contents, []),
    close(Boilerplate).

compile_expr(int(N), File) :-
    !,
    format(File, " ~w ", [N]).
compile_expr(float(X), File) :-
    !,
    format(File, " ~w ", [X]).
compile_expr(char(C), File) :-
    !,
    % TODO: escape sequences
    format(File, " '~w' ", [C]).
compile_expr(bool(B), File) :-
    !,
    format(File, " ~w ", [B]).
compile_expr(unit, File) :-
    !,
    format(File, " unit::unit ", []).
compile_expr(wildcard, File) :-
    !,
    format(File," __wildcard ", []).
compile_expr(id(Var), File) :-
    !,
    normalize_name(Var, NVar),
    format(File, " ~w ", [NVar]).
compile_expr(enum(Name), File) :-
    !,
    normalize_name(Name, NName),
    format(File, " __enum(\"~w\") ", [NName]).
compile_expr(adt(Name, Arg), File) :-
    !,
    format(File,  "__constructor(\"~w\",", [Name]),
    compile_expr(Arg, File),
    format(File, ")\n", []).


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

