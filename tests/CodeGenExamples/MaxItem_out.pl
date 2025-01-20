maxItem_lambda0(X,Y,_var2) :- 
	( X>=Y -> (
		_var3 = X
	)
	;(
		_var3 = Y
	)
	),
	_var4 = _var3,
	_var2 = _var4
	.
	

maxItem(List,_var0) :- 
	_var1 = maxItem_lambda0,
	Max = _var1,
	_var5 = [L|Ls],
	List = _var5,
	( length(Ls,0) -> (
		_var6 = L
	)
	;(
		maxItem(Ls,_var7),
		call(Max,_var7,L,_var8),
		_var6 = _var8
	)
	),
	_var9 = _var6,
	_var0 = _var9
	.


