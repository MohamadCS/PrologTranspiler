:- module(bin_heap,
    [
        heap_sort/2
    ]
).

% Binomial Tree of order 0
bt(_, []).

% Now for higher order bt(>=1), it must satisfy the following:
% All of its children are binomial trees of order i = k-1,...0,
% so over all k children, so what we want to check
% is the the i-th child is a binomial tree, and that K-i = order(child_i) = length(child_i list)
% So its enough to check wither the current tail of the root is equal to the child order.
bt(Value , [bt(ChildValue,ChildList) | RootListTail] ) :- 
    length(RootListTail, K), 
    length(ChildList, K),
    Value =< ChildValue,
    bt(Value,RootListTail)
    .

% merge_bt(X,Y,R) merge X and Y and the result is R, assuming the 
% trees are of the same order, here we have seperate cases, 
% depending on the which is the lower value, and the one with higher value
% will be its most left child
merge_bt(bt(Value1,List1), bt(Value2,List2), bt(Value1,[bt(Value2,List2) | List1])) :-
    Value2 > Value1
    .
merge_bt(bt(Value1,List1), bt(Value2,List2), bt(Value2,[bt(Value1,List1) | List2])) :-
    Value1 >= Value2
    .

merge_bt(X,empty, X) .
merge_bt(empty,X, X) .


% This is the function that the user will use
% Below we will define a helper function since we want 
% to keep track of the carry

merge_bin_heaps(Heap1,Heap2,ResultHeap) :-
    write("merge_bin_heaps\n"),
    merge_bin_heaps_(Heap1,Heap2,ResultHeap,empty)
    .



% If both lists are empty, and we still have a carry, then
% the result shoulld have that carry
merge_bin_heaps_([],[],[],empty).
merge_bin_heaps_([],[],[bt(V,L)],bt(V,L)) .

% If one of the heaps is empty, and there is no carry then the result should be
% the other heap(This includes the case that the other heap is not empty)
merge_bin_heaps_(Heap1, [], Heap1 ,empty).
merge_bin_heaps_([], Heap2, Heap2 ,empty).

% adding two heaps when one is empty, the other is not,
% and the current entry in the other heap is empty
merge_bin_heaps_([empty | HeapTail] ,[], [CarryBt | HeapTail], CarryBt).
merge_bin_heaps_([], [empty | HeapTail], [CarryBt | HeapTail], CarryBt). 


% adding two heaps when one is empty, the other is not,
% and the carry is not empty, so we merge the carry with the non-empty heap tree
% and continue 
merge_bin_heaps_([],[Y | Heap2Tail], [empty | ResultTail], CarryBt) :-
    merge_bt(Y,CarryBt,NewCarry), 
    merge_bin_heaps_([],Heap2Tail,ResultTail,NewCarry) 
    .

merge_bin_heaps_([X | Heap1Tail],[], [empty | ResultTail], CarryBt) :-
    merge_bt(X,CarryBt,NewCarry), 
    merge_bin_heaps_(Heap1Tail,[],ResultTail,NewCarry) 
    .

% Merging two heaps, one of them have an empty entry, and the carry is empty
merge_bin_heaps_([X | Heap1Tail], [empty | Heap2Tail], [X | ResultTail], empty) :-
    merge_bin_heaps_(Heap1Tail,Heap2Tail,ResultTail,empty)
    .
merge_bin_heaps_([empty | Heap1Tail], [X | Heap2Tail], [X | ResultTail], empty) :-
    merge_bin_heaps_(Heap1Tail,Heap2Tail,ResultTail,empty)
    .

% Merging two heaps, one of them have an empty entry, and the carry is empty
merge_bin_heaps_([X | Heap1Tail], [empty | Heap2Tail], [empty | ResultTail], CarryBt) :-
    merge_bt(X,CarryBt,NewCarry),
    merge_bin_heaps_(Heap1Tail,Heap2Tail,ResultTail,NewCarry)
    .
merge_bin_heaps_([empty | Heap1Tail], [X | Heap2Tail], [empty | ResultTail], CarryBt) :-
    merge_bt(X,CarryBt,NewCarry),
    merge_bin_heaps_(Heap1Tail,Heap2Tail,ResultTail,NewCarry)
    .
%
% This case is for X,Y,CarryBt are not empty, so heap.current = CarryBt, and
% we add merge X and Y, and they are the new carry

merge_bin_heaps_([X | Heap1Tail], [Y | Heap2Tail], [CarryBt | ResultTail],CarryBt) :- 
    merge_bt(X,Y,NewCarry),
    merge_bin_heaps_(Heap1Tail,Heap2Tail,ResultTail,NewCarry)
    .



% Prepare a new empty list of length order of the new bt
% then merge the two binomial heaps
add_bt(bt(Value,List), Heap, Result) :-
    length(List,K),
    length(EmptyList,K), % Prepare a list of length(List) .
    maplist(=(empty),EmptyList), % The list should have all empty 
    append(EmptyList,[bt(Value,List)],TreeHeap), % Prepare the heap, which has bt in the write index.
    merge_bin_heaps(TreeHeap,Heap,Result) 
    .

% Removes a bt from a heap and returns the updated heap.
%

remove_empty_suffix(X,Y) :-
    write("remove_empty_suffix\n"),
    remove_empty_suffix_(X,Y)
    .

remove_empty_suffix_([], []).
remove_empty_suffix_([empty], []).
remove_empty_suffix_([bt(V,L) | Xs], [bt(V,L)]) :- maplist(=(empty),Xs).
remove_empty_suffix_(X, []) :- maplist(=(empty),X).
remove_empty_suffix_([X | Xs], [X | Rs]) :-  remove_empty_suffix_(Xs,Rs).

remove_bt(Bt,Heap,Result) :- 
    remove_bt_(Bt,Heap,Result)
    .
% Base case 
remove_bt_(_,[],[]).
% Base case with one element in the list (For a Target Bt at the end of the list).
remove_bt_(TargetBt, [TargetBt], []). 


% If the Current Element is the TargetBt, replace it with empty
% and the rest of the heap should look like the original.
remove_bt_(TargetBt, [TargetBt | Xs], [empty | Xs]). 

% Otherwise (If the last case is not correct => Y != TargetBt)
% then continue
remove_bt_(TargetBt,[Y | Ys], [Y |Rs]) :- remove_bt_(TargetBt,Ys,Rs). 


% BT1 <= BT2 <=> Value1 <= Value2.
min_pred(_ , empty).
min_pred(bt(Value1,_),bt(Value2,_)) :- Value1 =< Value2. 



% BUG: The problem is when the last is bt and the others are empty

% pop_min(Heap, Bt, Result) gets the minimum heap
% from Heap, stores it in Bt, and 'returns'_(TempResult,Result) the updated
% heap in Result.

pop_min(Heap,bt(Value,List),ResultHeap) :-
    write("pop_min\n"),
    min_member(min_pred,bt(Value,List),Heap),
    remove_bt(bt(Value,List),Heap,TempHeap),
    remove_empty_suffix(TempHeap, TempHeap2),
    reverse(List,ChildrenHeap),
    merge_bin_heaps(TempHeap2,ChildrenHeap,ResultHeap),
    write("\n---------START----------\n"),
    write("\nHeap is\n"),
    write(Heap),
    write("\nTempHeap is\n"),
    write(TempHeap),
    write("\nTempHeap2 is\n"),
    write(TempHeap2),
    write("\nResultHeap is\n"),
    write(ResultHeap),
    write("\nChildrenHeap is\n"),
    write(ChildrenHeap),
    write("\n--------END-----------\n")
    .

% Given a list of numbers, returns the sorted list in Result.
heap_sort(List, Result) :- 
    list_to_heap(List,Heap),
    heap_to_sorted_list(Heap,Result)
    .

% Turns a binomial heap into a a sorted list
%
heap_to_sorted_list(X,Y) :- 
    write("heap_to_sorted_list\n"),
    heap_to_sorted_list_(X,Y)
    .

heap_to_sorted_list_([],[]).
heap_to_sorted_list_(Heap,[Value| Tail]) :-
    pop_min(Heap ,bt(Value,_), NewHeap),
    heap_to_sorted_list_(NewHeap, Tail)
    .

% Helper function, turns a list of number to a binomial heap heap
list_to_heap(X,Y) :- 
    write("heap_to_sorted_list\n"),
    list_to_heap_(X,Y)
    .

list_to_heap_([],[]).
list_to_heap_([X | Tail], Heap) :-
    list_to_heap_(Tail,TempHeap),
    add_bt(bt(X,[]),TempHeap, Heap)
    .
