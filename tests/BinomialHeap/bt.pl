:- module(bt,
    [
        bt/2,
        merge_bt/3,
        add_bt/3,
        add/3,
        get_min/2,
        pop_min/3,
        heap_sort/2
    ]
).

%%% bt
bt(_, [],0).

bt(V , [bt(V1,_) | T] , K) :- 
    K > 0,
    V < V1,
    K1 is K-1,
    bt( _ ,T,K1)
    .

bt(V,L) :- 
    length(L,K),
    bt(V,L,K) 
    .

%%% merge_bt

% V1 is the minimum
merge_bt(bt(V1,L1), bt(V2,L2), bt(V1,[bt(V2,L2) | L1])) :-
    length(L1,K),
    length(L2,K),
    V2 > V1 
    .

% V2 Is the minimum
merge_bt(bt(V1,L1), bt(V2,L2), bt(V2,[bt(V1,L1) | L2])) :-
    length(L1,K),
    length(L2,K),
    V1 >= V2 
    .

%%% add_bt

% If index is not the new tree's order then go to the next 
% element and increase index
%

add_bt(bt(V,L),[_| Xs], [_ | Rs],I) :-
    \+ length(L,I),
    I1 is I+1,
    add_bt(bt(V,L),Xs, Rs,I1)
    .

%If HeapList[order(tree)] is empty, then replace it with the tree and stop. 
add_bt(bt(V,L),[empty | Xs], [bt(V,L)| Xs],I) :-
    length(L,I)
    .

%If HeapList[order(tree)] is not empty , Result[order(tree)] = empty 
%since we will add two ones and Add the remainder recursivly.
add_bt(bt(V,L),[bt(V2,L2)| Xs], [empty | Rs],I) :-
    length(L,I),
    I1 is I+1,
    merge_bt(bt(V,L),bt(V2,L2), T),
    add_bt(T,Xs,Rs,I1)
    .

% This case happens when we need to increase the size of the list 
% duo to the creation of a bigger order tree than all of the biggest of
% the original heap.
add_bt(bt(V,L),[], [bt(V,L)],I) :-
    length(L,I)
    .

add_bt(bt(V,L),[], [],I) :-
    \+ length(L,I)
    .

add_bt(bt(V,L), X, R) :- 
    I is 0,
    add_bt(bt(V,L),X,R,I)
    .

%% For adding a list of bt.

add_bt([],R,R).  

add_bt([bt(V,L)],H,R) :- 
    add_bt(bt(V,L),H,R). 

add_bt([bt(V,L) | Ys],H,R) :- 
    add_bt(bt(V,L),H,R1), 
    add_bt(Ys,R1,R).


add([],HL,HL).

add([X],HL,R) :- add(X,HL,R).

add([X | Xs],HL, RL) :- 
    add(X,HL,R),
    add(Xs,R,RL)
    .

add(X,HL, RL) :- add_bt(bt(X,[]),HL,RL).



remove_bt(_,[],[]).
remove_bt(X, [X | Xs], [empty | Xs]). 
remove_bt(X,[Y | Ys], [Y|Rs]) :- remove_bt(X,Ys,Rs). 

get_min([bt(Vc,Lc) | Xs], bt(Vc,Lc)) :-
    get_min(Xs, bt(Vn,_)),
    Vn >= Vc.

get_min([bt(Vc,_) | Xs], bt(Vn,Ln)) :-
    get_min(Xs, bt(Vn,Ln)),
    Vc > Vn.

get_min([], empty).
get_min([X | Xs], X) :- get_min(Xs, empty).
get_min([empty | Xs], R) :- get_min(Xs, R).



remove_empty([empty],[]).
remove_empty([X | Xs],[ X | Ys]) :-  remove_empty(Xs,Ys).

pop_min(X,H,Y) :-
    get_min(H,bt(X,L)),
    remove_bt(bt(X,L),H,R),
    remove_empty(R,R1),
    add_bt(L,R1,Y)
    .


heap_sort([],[]).

heap_sort(L,SL) :- 
    add(L,[],R1), 
    heap_to_list(R1,SL)
    .

heap_to_list([],[]). 

heap_to_list(HL,[V | Vs]) :-
    pop_min(V ,HL, R),
    heap_to_list(R, Vs)
    .







