:- module(
	std,
	[

		forEach/3	]
)
.
testForEach_lambda0(X,_var2) :- 
	_var3 is X*X,
	_var4 = _var3,
	_var2 = _var4
	.
	

forEach(List,Func,_var0) :- 
	( length(List,0) -> (
		_var2 = [],
		_var3 = _var2,
		_var1 = _var3
	)
	;(
		List=[L|Ls],
		forEach(Ls,Func,_var4),
		Rs = _var4,
		call(Func,L,_var5),
		R = _var5,
		_var6 = [R|Rs],
		_var7 = _var6,
		_var1 = _var7
	)
	),
	_var8 = _var1,
	_var0 = _var8
	.


testForEach(L,_var0) :- 
	_var1 = testForEach_lambda0,
	std:forEach(L,_var1,_var5),
	_var6 = _var5,
	_var0 = _var6
	.


