:- module(
	testing,
	[

	]
)
.

eXPECT_EQ(X,Y,_var0) :- 
	( X=Y -> (
		_var2 = "SUCCESS",
		std:printLn(_var2,_var3),
		_var4 = tuple(  ),
		_var1 = _var4
	)
	;(
		_var5 = "FAILED",
		std:printLn(_var5,_var6),
		_var7 = tuple(  ),
		_var1 = _var7
	)
	),
	_var8 = tuple(  ),
	_var0 = _var8
	.


eXPECT_NEQ(X,Y,_var0) :- 
	( X\=Y -> (
		_var2 = "SUCCESS",
		std:printLn(_var2,_var3),
		_var4 = tuple(  ),
		_var1 = _var4
	)
	;(
		_var5 = "FAILED",
		std:printLn(_var5,_var6),
		_var7 = tuple(  ),
		_var1 = _var7
	)
	),
	_var8 = tuple(  ),
	_var0 = _var8
	.


