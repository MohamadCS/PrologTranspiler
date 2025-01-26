
factorial1(N,_var0) :- 
	( N=<1 -> (
		_var2 is 1,
		_var3 = _var2,
		_var1 = _var3
	)
	;(
		_var4 is N-1,
		factorial1(_var4,_var5),
		
		X = _var5,
		_var6 is X*N,
		_var7 = _var6,
		_var1 = _var7
	)
	),
	_var8 = _var1,
	_var0 = _var8
	.


factorial2(N,_var0) :- 
	( N=<1 -> (
		_var2 is 1,
		
		R = _var2,
		_var3 = tuple(  ),
		_var1 = _var3
	)
	;(
		_var4 is N-1,
		factorial2(_var4,_var5),
		
		X = _var5,
		_var6 is X*N,
		_var7 = _var6,
		
		R = _var7,
		_var8 = tuple(  ),
		_var1 = _var8
	)
	),
	_var9 = R,
	_var0 = _var9
	.


