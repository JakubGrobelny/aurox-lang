:- [lexer].
:- op(1200,xfx,==>).

parse_file(FileName, AST) :-
    tokenize_file(FileName, Tokens),
    catch(
        phrase(parser(AST), Tokens), 
        Error at Pos, 
        handle_parsing_error(Error, FileName, Pos)
    ).

handle_parsing_error(Error, FileName, Pos) :-
    print_error(Pos, FileName, '~w', [Error]),
    halt.


parser([]) --> [].