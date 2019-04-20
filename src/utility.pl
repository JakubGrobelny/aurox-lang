print_error(pos(L, C), FileName, MessageFormat, MessageArgs) :-
    atomic_list_concat(
        ['~w:~w:~w \u001b[31;1merror:\x1B[0m ', MessageFormat, '\n'],
        FinalFormat
    ),
    format(FinalFormat, [FileName, L, C | MessageArgs]),
    halt.