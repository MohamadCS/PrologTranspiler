
f_lambda0(X,L,_var2) :- 
	number(X),
	list(L),
	_var3 is 1,
	_var4 = _var3,
	_var2 = _var4
	.
	

max(X,Y,_var0) :- 
	number(X),
	number(Y),
	( X>=Y -> (
		_var1 = X
	)
	;(
		_var1 = Y
	)
	),
	_var2 = _var1,
	_var0 = _var2
	.


f(_var0) :- 
	_var1 = f_lambda0,
	_var5 = _var1,
	_var0 = _var5
	.


