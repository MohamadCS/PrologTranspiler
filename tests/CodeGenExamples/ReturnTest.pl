
g_lambda0(X,Y,_var5) :- 
	_var6 is X+Y,
	_var7 = _var6,
	_var5 = _var7
	.
	

f(X,_var0) :- 
	( (X==1 -> (
		_var1 is 0,
		_var0 = _var1,
		!,
		_var2 = tuple(  ),
		true))
	);
	_var3 is 5,
	_var4 = _var3,
	_var0 = _var4
	.


g(X,_var0) :- 
	( (X==1 -> (
		_var1 is 1,
		_var2 is 2,
		_var3 is 3,
		_var4 = g_lambda0,
		_var8 = tuple( _var1,_var2,_var3,_var4 ),
		_var0 = _var8,
		!,
		_var9 = tuple(  ),
		true))
	);
	_var10 is 1,
	_var11 = _var10,
	_var0 = _var11
	.


h(X,Y,_var0) :- 
	( (X==1 -> (
		_var1 is 1,
		_var0 = _var1,
		!,
		_var2 = tuple(  ),
		true))
	);
	( (X==1 -> (
		( (Y==1 -> (
			_var3 is 5,
			_var0 = _var3,
			!,
			_var4 = tuple(  ),
			true))
		);
		_var5 = tuple(  ),
		true))
	);
	( X+Y>100 -> (
		_var7 is 100,
		_var0 = _var7,
		!,
		_var8 = tuple(  ),
		_var6 = _var8
	)
	;(
		_var9 is 10,
		_var0 = _var9,
		!,
		_var10 = tuple(  ),
		_var6 = _var10
	)
	),
	_var11 = [],
	_var12 = _var11,
	_var0 = _var12
	.


