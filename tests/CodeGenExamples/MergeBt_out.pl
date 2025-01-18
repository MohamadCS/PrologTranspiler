bt(_,[]).
bt(Value,[bt(ChildValue,ChildList)|RootListTail]):-length(RootListTail,K),length(ChildList,K),Value=<ChildValue,bt(Value,RootListTail).
bt(Value,List,Var0) :- 
Var1 = bt(Value,List),
Var2 = Var1,
Var0 = Var2
.
mergeBt(Bt1,Bt2,Var3) :- 
bt(Value1,List1,Var4),
Bt1 = Var4,
bt(Value2,List2,Var5),
Bt2 = Var5,
(Value1>=Value2 -> (
Var7 = [Bt1|List2],
bt(Value2,Var7,Var8),
Var9 = Var8,
Var6 = Var9)
;(
Var10 = [Bt2|List1],
bt(Value1,Var10,Var11),
Var12 = Var11,
Var6 = Var12)
),
Var13 = Var6,
Var3 = Var13
.
mergeBt2(Bt1,Bt2,Var14) :- 
Var15 = bt(Value1,List1),
Bt1 = Var15,
Var16 = bt(Value2,List2),
Bt2 = Var16,
(Value1>=Value2 -> (
Var18 = bt(Value2,[Bt1|List2]),
Var19 = Var18,
Var17 = Var19)
;(
Var20 = bt(Value1,[Bt2|List1]),
Var21 = Var20,
Var17 = Var21)
),
Var22 = Var17,
Var14 = Var22
.
