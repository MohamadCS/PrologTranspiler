fib(N,Var0) :- 
(N=<1 -> (
Var2 = N,
R = Var2,
Var3 = std_tuple(  ),
Var1 = Var3)
;(
N1 is N-1,
N2 is N-2,
Var7 = N1,
fib(Var7,Var8),
Var6 = Var8,
F1 = Var6,
Var10 = N2,
fib(Var10,Var11),
Var9 = Var11,
F2 = Var9,
X is (F1+F2),
Var13 = X,
R = Var13,
Var14 = std_tuple(  ),
Var1 = Var14)
),
Var15 = R,
Var16 = Var15,
Var0 = Var16
.
