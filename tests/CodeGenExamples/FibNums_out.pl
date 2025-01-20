fib1(N,Var0) :- 
( (N=0 -> (
Var1 is 0,
R = Var1,
Var2 = tuple(  ),
true);true)),
( (N=1 -> (
Var3 is 1,
R = Var3,
Var4 = tuple(  ),
true);true)),
( (N>1 -> (
Var5 is N-1,
fib1(Var5,Var6),
F1 = Var6,
Var7 is N-2,
fib1(Var7,Var8),
F2 = Var8,
Var9 is F1+F2,
Var10 = Var9,
R = Var10,
Var11 = tuple(  ),
true);true)),
Var12 = R,
Var0 = Var12
.
fib2(N,Var0) :- 
(N=<1 -> (
Var2 = N,
Var1 = Var2)
;(
Var3 is N-1,
fib2(Var3,Var4),
F1 = Var4,
Var5 is N-2,
fib2(Var5,Var6),
F2 = Var6,
Var7 is F1+F2,
Var8 = Var7,
Var1 = Var8)
),
Var9 = Var1,
Var0 = Var9
.
