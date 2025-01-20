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
maxItem(List,Var5) :- 
Var6 = [L|Ls],
List = Var6,
(length(Ls,0) -> (
Var8 = L,
Var7 = Var8)
;(
maxItem(Ls,Var9),
T = Var9,
max(T,L,Var10),
Var11 = Var10,
Var7 = Var11)
),
Var12 = Var7,
Var5 = Var12
.
