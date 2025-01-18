factorial(N,Var0) :- 
(N=<1 -> (
R is 1,
Var2 = R,
Var1 = Var2)
;(
Var3 is N-1,
factorial(Var3,Var4),
X = Var4,
R is (X*N),
Var5 = R,
Var1 = Var5)
),
Var6 = Var1,
Var0 = Var6
.
