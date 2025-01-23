:- module(
	test,
	[

	]
)
.

:- use_module('std.pl').
test:testForEach_lambda0(X,_var3) :- 
	_var4 is X*X,
	_var5 = _var4,
	_var3 = _var5
	.
	

test:testForEach(_var0) :- 
	_var1 = [1,2,3,4],
	_var2 = test:testForEach_lambda0,
	std:forEach(_var1,_var2,_var6),
	_var7 = _var6,
	_var0 = _var7
	.


test:testSize(_var0) :- 
	_var1 = [1,2,3],
	std:size(_var1,_var2),
	_var3 = [],
	std:size(_var3,_var4),
	_var5 = [1],
	std:size(_var5,_var6),
	_var7 = tuple( _var2,_var4,_var6 ),
	_var0 = _var7
	.


test:testReplace(_var0) :- 
	_var1 = [1,2,3],
	_var2 is 1,
	_var3 is 100,
	std:replace(_var1,_var2,_var3,_var4),
	_var5 = _var4,
	_var0 = _var5
	.


