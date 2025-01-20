max(X,Y,Var0) :- 
(X>=Y -> (
Var1 = X)
;(
Var1 = Y)
),
Var2 = Var1,
Var0 = Var2
.
maxTree(Root,Var0) :- 
Var1 = node(V,LeftChild,RightChild),
Root = Var1,
(LeftChild=nil,RightChild=nil -> (
Var3 = V,
Var2 = Var3)
;(
(RightChild=nil -> (
maxTree(LeftChild,Var5),
max(V,Var5,Var6),
Var7 = Var6,
Var4 = Var7)
;(
(LeftChild=nil -> (
maxTree(RightChild,Var9),
max(V,Var9,Var10),
Var11 = Var10,
Var8 = Var11)
;(
maxTree(RightChild,Var12),
maxTree(LeftChild,Var13),
max(Var12,Var13,Var14),
MaxChild = Var14,
max(MaxChild,V,Var15),
Var16 = Var15,
Var8 = Var16)
),
Var4 = Var8)
),
Var2 = Var4)
),
Var17 = Var2,
Var0 = Var17
.
