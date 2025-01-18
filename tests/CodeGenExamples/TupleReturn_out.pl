f(List,Var0) :- 
List=[L|Ls],
(length(Ls,0) -> (
Var2 is 0,
Var3 = tuple( L,Var2 ),
Var1 = Var3)
;(
Var4 is 1,
Var5 = tuple( L,Var4 ),
Var1 = Var5)
),
Var6 = Var1,
Var0 = Var6
.
