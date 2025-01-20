lambda0(X,Y,Var2) :- 
Var3 is X+Y,
Var4 = Var3,
Var2 = Var4
.
f(X,Var0) :- 
Var1 is 1,
Var2 is 2,
call(X,Var1,Var2,Var3),
Var4 = Var3,
Var0 = Var4
.
g(X,Var0) :- 
Var1 = lambda0,
f(Var1,Var5),
Var6 = Var5,
Var0 = Var6
.
