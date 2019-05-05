:- ensure_loaded(utility).
:- ensure_loaded(environment).


% How to do type checking?
% go through all variables in environment
% keep a list of visited variables (not needed actually)
% if type was defined, then try to prove it
% else infer the type
% inferrence failure -> error
% type mismatch -> error 
% type does not match the signature -> error

typecheck_environment(env(Env, TEnv)) :-
    dict_pairs(Env, _, Contents),
    typecheck_environment(Contents, env(Env, TEnv)).

typecheck_environment([], _, _) :- !.
typecheck_environment([_-(Val at Pos, Type, _) | Vars], Env) :-
    infer_type(Env, Val, Type, Pos),
    !,
    typecheck_environment(Vars, Env).
% TODO: case when Var is a Constructor
typecheck_environment([Var-(Val at ValPos, Type, Pos) | _], Env) :-
    \+ var(Type),
    infer_type(Env, Val, ValType, Pos),
    !,
    print_type_error(
        ValPos,
        'The type ~w of expression does not match the type annotation ~w specified\c
         in the definition of ~w',
        [type(ValType), type(Type), Var]
    ).
typecheck_environment([Var-(_ at ValPos, _, _) | _], _) :-
    print_type_error(
        ValPos,
        'the type of value of ~w couldn\'t have been inferred',
        [Var]
    ).

infer_type(env(Env, _), id(Var), ExpectedType, _) :-
    get_dict(Var, Env, (_, Type, _)),
    \+ var(Type),
    !,
    copy_term(Type, ExpectedType).
infer_type(env(Env, TEnv), id(Var), ExpectedType, _) :-
    get_dict(Var, Env, (Val, Type, DefPos)),
    !,
    infer_type(env(Env, TEnv), Val, Type, DefPos),
    copy_term(Type, ExpectedType).
infer_type(_, int(_), adt('Int', []), _) :- !.
infer_type(_, float(_), adt('Float', []), _) :- !.
infer_type(_, char(_), adt('Char', []), _) :- !.
infer_type(_, bool(_), adt('Bool', []), _) :- !.
infer_type(_, unit, adt('Unit', []), _) :- !.


print_type_error(Start, MsgFormat, MsgArgs) :-
    prettify_types(MsgArgs, Prettified),
    print_error_and_halt(Start, MsgFormat, Prettified).

prettify_types([], []) :- !.
prettify_types([type(T) | Ts], [PT | PTs]) :-
    !,
    prettify_type(T, PT),
    prettify_types(Ts, PTs).
prettify_types([Anything | Ts], [Anything | PTs]) :-
    prettify_types(Ts, PTs).


prettify_type(X, X) :- !. % TODO: implement

extract_variables_from_type(none, []) :- !.
extract_variables_from_type(Var, []) :-
    var(Var),
    !.
extract_variables_from_type(param(A), [A]) :- !.
extract_variables_from_type(name(_), []) :- !.
extract_variables_from_type(adt(_, Parameters), FilteredParams) :-
    !,
    extract_variables_from_type(Parameters, FilteredParams).
extract_variables_from_type([T | Types], Params) :-
    !,
    extract_variables_from_type(T, TParams), 
    extract_variables_from_type(Types, RestParams),
    append(TParams, RestParams, Params).
extract_variables_from_type([], []) :- !.
extract_variables_from_type(tuple(_, Types), Params) :-
    !,
    extract_variables_from_type(Types, Params).
extract_variables_from_type(T, Params) :-
    T =.. [_ | Args],
    extract_variables_from_type(Args, Params).
