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
	

length(List,_var0) :- 
	_var1 = length(List,K),
	length(List,K),
	_var2 = K,
	_var0 = _var2
	.


maxItem(List,_var0) :- 
	_var1 = maxItem_lambda0,
	Max = _var1,
	_var5 = [L|Ls],
	List = _var5,
	length(Ls,_var7),
	L = _var7,
	( L=0 -> (
		_var8 = L,
		_var6 = _var8
	)
	;(
		maxItem(Ls,_var9),
		T = _var9,
		call(Max,T,L,_var10),
		_var11 = _var10,
		_var6 = _var11
	)
	),
	_var12 = _var6,
	_var0 = _var12
	.


