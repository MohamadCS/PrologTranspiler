 




and(X,Y,false) :-  \+ X ; \+ Y.
and(X,Y,true) :-  X,Y.

or(X,Y,false) :-  \+ X , \+ Y.
or(X,Y,true) :-  X;Y.




And(X,Y) :: (
    and(X,Y,Z);
    Z
).

Or(X,Y) :: (
    or(X,Y,Z);
    Z
).


IsBt(Value,Children) :: (
    Children is [[ChildValue,ChildList] | Tail];
    length(Tail, K);  
    length(ChildList,K);
    RootLessThanChild <- Value =< ChildValue;
    TailIsHeap <- IsBt(Value, Tail);
    And(RootLessThanChild,TailIsHeap)
).

MergeBt(Bt1, Bt2) :: ( 
)


    





