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
factorial2(N,Var0) :- 
(N=<1 -> (
Var2 is 1,
R = Var2,
Var3 = tuple(  ),
Var1 = Var3)
;(
Var4 is N-1,
factorial2(Var4,Var5),
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
