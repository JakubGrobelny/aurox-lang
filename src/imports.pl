:- ensure_loaded(operators).
:- ensure_loaded(parser).
:- ensure_loaded(utility).

import_dependencies([], Deps, EmptyOps, Deps, X, X) :-
    empty_operator_list(EmptyOps),
    !.
import_dependencies(
    [file_name(File) at FStart | Deps],
    AlreadyImported,
    Operators,
    FinalDeps,
    OutputProgram,
    X
) :-
    \+ member(file_name(File), AlreadyImported),
    !,
    fix_file_path(File, FixedFile),
    parse_file(
        FixedFile, 
        DepAST, 
        FStart, 
        AlreadyImported,
        NewlyImported,
        FileOperators
    ),
    import_dependencies(
        Deps,
        [file_name(File) | NewlyImported],
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
    