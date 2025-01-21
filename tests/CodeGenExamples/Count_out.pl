main_lambda0(X,_var2) :- 
	X>=1,
	_var3 = tuple(  ),
	_var2 = _var3
	.
	

count(List,Func,_var0) :- 
	( length(List,0) -> (
		_var2 is 0,
		_var3 = _var2,
		_var1 = _var3
	)
	;(
		_var4 = [L|Ls],
		List = _var4,
		count(Ls,Func,_var5),
		I = _var5,
		call(Func,L,_var6),
		Condition = _var6,
		( Condition -> (
			_var8 is I+1,
			_var9 = _var8,
			_var7 = _var9
		)
		;(
			_var10 = I,
			_var7 = _var10
		)
		),
		_var11 = _var7,
		_var1 = _var11
	)
	),
	_var12 = _var1,
	_var0 = _var12
	.


main(_var0) :- 
	_var1 = main_lambda0,
	Condition = _var1,
	_var4 = [1,3,4,3],
	count(_var4,Condition,_var5),
	Result = _var5,
	_var6 = write(Result),
	_var7 = _var6,
	_var0 = _var7
	.


