
:- use_module('std.pl').
testForEach_lambda0(X,_var3) :- 
	_var4 is X+X,
	_var5 = _var4,
	_var3 = _var5
	.
	

testForEach(_var0) :- 
	_var1 = [1,2,3,4,5],
	L = _var1,
	_var2 = testForEach_lambda0,
	std:forEach(L,_var2,_var6),
	_var7 = _var6,
	_var0 = _var7
	.


