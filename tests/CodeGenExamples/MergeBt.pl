
bt(_, []).

bt(Value , [bt(ChildValue,ChildList) | RootListTail] ) :- 
    length(RootListTail, K), 
    length(ChildList, K),
    Value =< ChildValue,
    bt(Value,RootListTail)
    .

Bt(Value,List) :: (
    bt(Value,List)
)
.

MergeBt(Bt1,  Bt2) :: (

    Bt1 <- Bt(Value1, List1);
    Bt2 <- Bt(Value2, List2);

    if Value1 >= Value2 then (
        Bt(Value2,[Bt1 | List2])
    ) else (
        Bt(Value1,[Bt2 | List1])
    )

)
.
MergeBt2(Bt1, Bt2) :: (

    Bt1 <- bt(Value1, List1);
    Bt2 <- bt(Value2, List2);

    if Value1 >= Value2 then (
        bt(Value2,[Bt1 | List2])
    ) else (
        bt(Value1,[Bt2 | List1])
    )

)
.












