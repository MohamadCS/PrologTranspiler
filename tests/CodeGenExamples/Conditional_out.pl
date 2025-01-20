max(X,Y,Var0) :- 
(X>=Y -> (
Var1 = X)
;(
Var1 = Y)
),
Var2 = Var1,
Var0 = Var2
.
