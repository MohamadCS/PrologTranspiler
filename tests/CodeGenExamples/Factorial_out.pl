factorial(N,Var0) :- 
(N=:=1;N=:=0 -> (
R is 1,
Var3 = std_tuple(  ),
Var1 = Var3)
;(
N1 is N-1,
Var6 = N1,
factorial(Var6,Var7),
Var5 = Var7,
X = Var5,
R is (X*N),
Var9 = std_tuple(  ),
Var1 = Var9)
),
Var10 = R,
Var11 = Var10,
Var0 = Var11
.
