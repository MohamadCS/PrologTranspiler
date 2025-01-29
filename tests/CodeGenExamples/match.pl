
:- use_module('std.pl').
foo(List,_var0) :- 
	std:size(List,_var2),
	_var3 is 1,
	_var4 is 2,
	(
	_var2 = _var3 -> 
	_var5 is 3,
	_var6 is 4,
	std:max(_var5,_var6,_var7),
	_var1 = _var7
	;
	_var2 = _var4 -> 
	_var8 = "2",
	std:print(_var8,_var9),
	_var1 = _var9
	;
	_var10 = "Hello",
	std:print(_var10,_var11),
	_var1 = _var11
	),
	_var12 = _var1,
	_var0 = _var12
	.


bar(X,_var0) :- 
	(
	_var1 = nil
	),
	Y = _var1,
	std:print(Y,_var2),
	_var4 is 1,
	_var5 is 2,
	(
	X = _var4 -> 
	_var6 is 3,
	_var3 = _var6
	;
	X = _var5 -> 
	_var7 is 4,
	_var3 = _var7
	;
	_var3 = nil
	),
	Z = _var3,
	std:print(Z,_var8),
	_var9 = tuple(  ),
	_var0 = _var9
	.


