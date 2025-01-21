maxTree_lambda0(X,Y,_var2) :- 
( X>=Y -> (
_var3 = X
)
;(
_var3 = Y
)
),
_var4 = _var3,
_var2 = _var4
.


maxTree(Root,_var0) :- 
_var1 = maxTree_lambda0,
Max = _var1,
_var5 = node(V,LeftChild,RightChild),
Root = _var5,
( LeftChild=nil,RightChild=nil -> (
_var7 = V,
_var6 = _var7
)
;(
( RightChild=nil -> (
maxTree(LeftChild,_var9),
call(Max,V,_var9,_var10),
_var11 = _var10,
_var8 = _var11
)
;(
( LeftChild=nil -> (
maxTree(RightChild,_var13),
call(Max,V,_var13,_var14),
_var15 = _var14,
_var12 = _var15
)
;(
maxTree(RightChild,_var16),
maxTree(LeftChild,_var17),
call(Max,_var16,_var17,_var18),
MaxChild = _var18,
call(Max,MaxChild,V,_var19),
_var20 = _var19,
_var12 = _var20
)
),
_var8 = _var12
)
),
_var6 = _var8
)
),
_var21 = _var6,
_var0 = _var21
.


