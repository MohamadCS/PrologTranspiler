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
fib2(N,Var13) :- 
(N=<1 -> (
Var15 = N,
Var14 = Var15)
;(
Var16 is N-1,
fib2(Var16,Var17),
F1 = Var17,
Var18 is N-2,
fib2(Var18,Var19),
F2 = Var19,
Var20 is F1+F2,
Var21 = tuple(  ),
Var14 = Var21)
),
Var22 = Var14,
Var13 = Var22
.
