:- module(
	std,
	[

		forEach/3,
		size/2	]
)
.

std:forEach(List,Func,_var0) :- 
	( length(List,0) -> (
		_var2 = [],
		_var3 = _var2,
		_var1 = _var3
	)
	;(
		_var4 = [L|Ls],
		List = _var4,
		std:forEach(Ls,Func,_var5),
		Rs = _var5,
		call(Func,L,_var6),
		R = _var6,
		_var7 = [R|Rs],
		_var8 = _var7,
		_var1 = _var8
	)
	),
	_var9 = _var1,
	_var0 = _var9
	.


std:privateFunc(_var0) :- 
	_var1 is 1,
	_var2 = _var1,
	_var0 = _var2
	.


std:size(List,_var0) :- 
	( List=[] -> (
		_var2 is 0,
		_var3 = _var2,
		_var1 = _var3
	)
	;(
		_var4 = [L|Ls],
		List = _var4,
		std:size(Ls,_var5),
		X = _var5,
		_var6 is X+1,
		_var7 = _var6,
		_var1 = _var7
	)
	),
	_var8 = _var1,
	_var0 = _var8
	.


