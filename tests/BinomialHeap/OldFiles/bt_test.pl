:- use_module('bin_heap.pl').

generate_random_list(0,[]).
generate_random_list(N,[X | Xs]) :- 
    N > 0,
    N1 is N-1, 
    generate_random_list(N1,Xs),
    random(1,1000,X)
    .


:- 
    generate_random_list(50,L),
    write(L),
    sleep(1),
    sort(L,S),
    heap_sort(L,S)
    .


