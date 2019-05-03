:- op(200, xfx, at).

print_error(pos(FileName, L, C), MessageFormat, MessageArgs) :-
    atomic_list_concat(
        ['~w:~w:~w \u001b[31;1merror:\x1B[0m ', MessageFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [FileName, L, C | MessageArgs]).

print_error_and_halt(Pos, MsgFormat, MsgArgs) :-
    print_error(Pos, MsgFormat, MsgArgs),
    halt.