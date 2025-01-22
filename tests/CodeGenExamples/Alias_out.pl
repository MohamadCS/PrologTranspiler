first(List,X,_var0) :- 
	( (length(List,0) -> (
		_var1 = false,
		R = _var1,
		_var2 = tuple(  ),
		true);true)
	),
	( List=[X|_] -> (
		_var4 = true,
		R = _var4,
		_var5 = tuple(  ),
		_var3 = _var5
	)
	;(
		_var6 = false,
		R = _var6,
		_var7 = tuple(  ),
		_var3 = _var7
	)
	),
	_var8 = R,
	_var0 = _var8
	.


