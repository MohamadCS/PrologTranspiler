max(X,Y,Var0) :- 
(X>=Y -> (
Var2 = X,
Var1 = Var2)
;(
Var3 = Y,
Var1 = Var3)
),
Var4 = Var1,
Var0 = Var4
.
maxTree(Root,Var5) :- 
Var6 = node(V,LeftChild,RightChild),
Root = Var6,
(LeftChild=nil,RightChild=nil -> (
Result = V,
Var8 = tuple(  ),
Var7 = Var8)
;(
(RightChild=nil -> (
maxTree(RightChild,Var10),
max(V,Var10,Var11),
Result = Var11,
Var12 = tuple(  ),
Var9 = Var12)
;(
(LeftChild=nil -> (
maxTree(LeftChild,Var14),
max(V,Var14,Var15),
Result = Var15,
Var16 = tuple(  ),
Var13 = Var16)
;(
maxTree(RightChild,Var17),
maxTree(LeftChild,Var18),
max(Var17,Var18,Var19),
T = Var19,
max(T,V,Var20),
Result = Var20,
Var21 = tuple(  ),
Var13 = Var21)
),
Var9 = Var13)
),
Var7 = Var9)
),
Var22 = Result,
Var5 = Var22
.
