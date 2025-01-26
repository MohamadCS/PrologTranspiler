
:- use_module('std.pl').
test_lambda0(X,Y,_var3) :- 
	( X=<Y -> (
		_var4 = X
	)
	;(
		_var4 = Y
	)
	),
	_var5 = _var4,
	_var3 = _var5
	.
	

test(_var0) :- 
	_var1 = [1,2,3,1],
	_var2 = test_lambda0,
	std:minMember(_var1,_var2,_var6),
	std:print(_var6,_var7),
	_var8 = tuple(  ),
	_var0 = _var8
	.


