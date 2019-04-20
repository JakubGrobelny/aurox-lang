:- [lexer].
:- op(1200,xfx,==>).

parse_file(FileName, AST) :-
    tokenize_file(FileName, Tokens),
    catch(
        phrase(parser(AST), Tokens), 
        error(Format, Args) at Pos, 
        print_error(Pos, FileName, Format, Args)
    ).

parser([]) --> [].