:- ensure_loaded(operators).
:- ensure_loaded(parser).
:- ensure_loaded(utility).

import_dependencies([], Deps, Ops, Deps, X, X, Ops) :- !.
import_dependencies(
    [module_name(Name) at FStart | Deps], 
    AlreadyImported,
    RestOps, 
    FinalImports, 
    OutputProgram,
    X,
    PrevOps
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
        PrevOps,
        ModuleOps
    ),
    import_dependencies(Deps, NewImports, RestOps, FinalImports, RestAST, X, ModuleOps),
    append(ModuleAST, RestAST, OutputProgram).
import_dependencies(
    [file_name(File) at pos(F, L, C) | Deps],
    AlreadyImported,
    RestOperators,
    FinalDeps,
    OutputProgram,
    X,
    PrevOps
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
        PrevOps,
        FileOperators
    ),
    import_dependencies(
        Deps,
        NewlyImported,
        RestOperators,
        FinalDeps,
        RestProgram,
        X,
        FileOperators
    ),
    append(DepAST, RestProgram, OutputProgram).
import_dependencies(
    [_ | Deps],
    Imported,
    Operators,
    DepsAcc,
    OutputProgram,
    X,
    PrevOps
) :-
    import_dependencies(
        Deps, 
        Imported,
        Operators,
        DepsAcc,
        OutputProgram,
        X,
        PrevOps
    ). 
    