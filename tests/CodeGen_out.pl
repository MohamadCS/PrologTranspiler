max(X,Y,X):-X>=Y.
max(X,Y,Y):-Y>X.

maxList([X],X).
maxList(List,Var0) :- 
List=[L|Ls],
Var3 = Ls,
maxList(Var3,Var4),
Var2 = Var4,
Current = Var2,
max(Current,L,R),
Var6 = R,
Var7 = tpl( Var6 ),
Var0 = Var7
.
