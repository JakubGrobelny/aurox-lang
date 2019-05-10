:- ensure_loaded(operators).
:- ensure_loaded(parser).
:- ensure_loaded(utility).

import_dependencies([], Deps, EmptyOps, Deps, X, X) :-
    empty_operator_list(EmptyOps),
    !.
import_dependencies(
    [module_name(Name) at FStart | Deps], 
    AlreadyImported,
    Operators, 
    FinalImports, 
    OutputProgram,
    X
) :-
    \+ member(module_name(Name), AlreadyImported),
    !,
    atomic_list_concat(['modules/' ,Name, '.ax'], FileName),
    fix_file_path(FileName, FixedFileName),
    parse_file(
        FixedFileName,
        ModuleAST,
        FStart,
        [module_name(Name) | AlreadyImported],
        NewImports,
        ModuleOps
    ),
    import_dependencies(Deps, NewImports, RestOps, FinalImports, RestAST, X),
    append(ModuleAST, RestAST, OutputProgram),
    merge_operators(ModuleOps, RestOps, Operators).
import_dependencies(
    [file_name(File) at pos(F, L, C) | Deps],
    AlreadyImported,
    Operators,
    FinalDeps,
    OutputProgram,
    X
) :-
    \+ member(file_name(File), AlreadyImported),
    !,
    fix_relative_path(F, File, FixedFileName),
    parse_file(
        FixedFileName, 
        DepAST, 
        pos(F, L, C), 
        [file_name(File) | AlreadyImported],
        NewlyImported,
        FileOperators
    ),
    import_dependencies(
        Deps,
        NewlyImported,
        RestOperators,
        FinalDeps,
        RestProgram,
        X
    ),
    append(DepAST, RestProgram, OutputProgram),
    merge_operators(FileOperators, RestOperators, Operators).
import_dependencies(
    [_ | Deps],
    Imported,
    Operators,
    DepsAcc,
    OutputProgram,
    X
) :-
    import_dependencies(
        Deps, 
        Imported,
        Operators,
        DepsAcc,
        OutputProgram,
        X
    ). 
    