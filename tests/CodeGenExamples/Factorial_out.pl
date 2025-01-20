factorial1(N,Var0) :- 
(N=<1 -> (
Var2 is 1,
Var3 = Var2,
Var1 = Var3)
;(
Var4 is N-1,
factorial1(Var4,Var5),
X = Var5,
Var6 is X*N,
Var7 = Var6,
Var1 = Var7)
),
Var8 = Var1,
Var0 = Var8
.
factorial2(N,Var9) :- 
(N=<1 -> (
Var11 is 1,
R = Var11,
Var12 = tuple(  ),
Var10 = Var12)
;(
Var13 is N-1,
factorial2(Var13,Var14),
X = Var14,
Var15 is X*N,
Var16 = Var15,
R = Var16,
Var17 = tuple(  ),
Var10 = Var17)
),
Var18 = R,
Var9 = Var18
.
