
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
		_var7 = L,
		_var6 = _var7
	)
	;(
		maxItem(Ls,_var8),
		T = _var8,
		call(Max,T,L,_var9),
		_var10 = _var9,
		_var6 = _var10
	)
	),
	_var11 = _var6,
	_var0 = _var11
	.


