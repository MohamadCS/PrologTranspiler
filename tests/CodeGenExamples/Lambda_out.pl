g1_lambda0(X,Y,Var2) :- 
Var3 is X+Y,
Var4 = Var3,
Var2 = Var4
.
g2_lambda0(X,Y,Var2) :- 
Var3 is X+Y,
Var4 = Var3,
Var2 = Var4
.
f(Func,Var0) :- 
Var1 is 1,
Var2 is 2,
call(Func,Var1,Var2,Var3),
Var4 = Var3,
Var0 = Var4
.
g1(Var0) :- 
Var1 = g1_lambda0,
f(Var1,Var5),
Var6 = Var5,
Var0 = Var6
.
g2(Var0) :- 
Var1 = g2_lambda0,
Add = Var1,
f(Add,Var5),
Var6 = Var5,
Var0 = Var6
.
