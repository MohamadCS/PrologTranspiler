max(X,Y,Var0) :- 
(X>=Y -> (
Var2 = X,
R = Var2,
Var3 = R,
Var1 = Var3)
;(
Var4 = Y,
R = Var4,
Var5 = R,
Var1 = Var5)
),
Var6 = R,
Var7 = Var6,
Var0 = Var7
.
maxItem(List,Var8) :- 
List=[L|Ls],
(length(Ls,0) -> (
Var11 = L,
R = Var11,
Var12 = std_tuple(  ),
Var10 = Var12)
;(
Var14 = Ls,
maxItem(Var14,Var15),
Var13 = Var15,
T = Var13,
(T>=L -> (
Var17 = T,
R = Var17,
Var18 = std_tuple(  ),
Var16 = Var18)
;(
Var19 = L,
R = Var19,
Var20 = std_tuple(  ),
Var16 = Var20)
),
Var21 = Var16,
Var10 = Var21)
),
Var22 = R,
Var23 = Var22,
Var8 = Var23
.
