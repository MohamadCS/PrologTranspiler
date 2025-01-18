factorial1(N,Var0) :- 
(N=<1 -> (
Var2 is 1,
R = Var2,
Var3 = tuple(  ),
Var1 = Var3)
;(
Var4 is N-1,
factorial1(Var4,Var5),
X = Var5,
Var6 is X*N,
Var7 = Var6,
R = Var7,
Var8 = tuple(  ),
Var1 = Var8)
),
Var9 = R,
Var0 = Var9
.
factorial2(N,Var10) :- 
(N=<1 -> (
Var12 is 1,
R = Var12,
Var13 = R,
Var11 = Var13)
;(
Var14 is N-1,
factorial2(Var14,Var15),
X = Var15,
Var16 is X*N,
Var17 = Var16,
R = Var17,
Var18 = R,
Var11 = Var18)
),
Var19 = Var11,
Var10 = Var19
.
