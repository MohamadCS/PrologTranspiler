max(X,Y,Var0) :- 
(X>=Y -> (
Var1 = X)
;(
Var1 = Y)
),
Var2 = Var1,
Var0 = Var2
.
maxTree(Root,Var3) :- 
Var4 = node(V,LeftChild,RightChild),
Root = Var4,
(LeftChild=nil,RightChild=nil -> (
Var6 = V,
Var5 = Var6)
;(
(RightChild=nil -> (
maxTree(LeftChild,Var8),
max(V,Var8,Var9),
Var10 = Var9,
Var7 = Var10)
;(
(LeftChild=nil -> (
maxTree(RightChild,Var12),
max(V,Var12,Var13),
Var14 = Var13,
Var11 = Var14)
;(
maxTree(RightChild,Var15),
maxTree(LeftChild,Var16),
max(Var15,Var16,Var17),
MaxChild = Var17,
max(MaxChild,V,Var18),
Var19 = Var18,
Var11 = Var19)
),
Var7 = Var11)
),
Var5 = Var7)
),
Var20 = Var5,
Var3 = Var20
.
