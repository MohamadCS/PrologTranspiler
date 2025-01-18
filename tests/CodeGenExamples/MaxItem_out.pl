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
Var6 = [L|Ls],
List = Var6,
(length(Ls,0) -> (
R = L,
Var8 = tuple(  ),
Var7 = Var8)
;(
maxItem(Ls,Var9),
T = Var9,
(T>=L -> (
R = T,
Var11 = tuple(  ),
Var10 = Var11)
;(
R = L,
Var12 = tuple(  ),
Var10 = Var12)
),
Var13 = tuple(  ),
Var7 = Var13)
),
Var14 = R,
Var5 = Var14
.
