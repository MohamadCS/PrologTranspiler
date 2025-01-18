max(X,Y,Var0) :- 
(X>=Y -> (
R = X,
Var2 = tuple(  ),
Var1 = Var2)
;(
R = Y,
Var3 = tuple(  ),
Var1 = Var3)
),
Var4 = R,
Var0 = Var4
.
maxItem(List,Var5) :- 
List=[L|Ls],
(length(Ls,0) -> (
R = L,
Var7 = tuple(  ),
Var6 = Var7)
;(
maxItem(Ls,Var8),
T = Var8,
(T>=L -> (
R = T,
Var10 = tuple(  ),
Var9 = Var10)
;(
R = L,
Var11 = tuple(  ),
Var9 = Var11)
),
Var12 = tuple(  ),
Var6 = Var12)
),
Var13 = R,
Var5 = Var13
.
