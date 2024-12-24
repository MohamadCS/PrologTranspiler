:- use_module('bt.pl').

generate_random_list(0,[]).
generate_random_list(N,[X | Xs]) :- 
    N > 0,
    N1 is N-1, 
    generate_random_list(N1,Xs),
    random(1,1000,X)
    .


:- bt(5, [bt(7, [])]).

:- \+ bt(7, [bt(4, [])]).

:- merge_bt(
    bt(3, [bt(4, [])]),
    bt(5, [bt(7, [])]),
    bt(3, [bt(5, [bt(7, [])]), bt(4, [])])
    ).

:- add_bt(bt(5, []), [bt(4, [])], [empty, bt(4, [bt(5, [])])]).

:- add(3, [bt(4, [])], [empty, bt(3, [bt(4, [])])]).

:- get_min([bt(2, []), bt(3, [bt(4, [])])],bt(2, [])).

:- get_min([empty, bt(3, [bt(4, [])])],bt(3, [bt(4, [])])).

:- get_min([empty, bt(3, [bt(4, [])])],bt(3, [bt(4, [])])).


:- 
    generate_random_list(10,L),
    heap_sort(L,S)
    .


