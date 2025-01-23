:- module(
	std,
	[

		forEach/3,
		size/2,
		exit/1,
		replace/4,
		max/3	]
)
.

std:forEach(List,Func,_var1) :- 
	std:size(List,_var3),
	Length = _var3,
	( Length=0 -> (
		_var4 = [],
		_var5 = _var4,
		_var2 = _var5
	)
	;(
		_var6 = [L|Ls],
		List = _var6,
		std:forEach(Ls,Func,_var7),
		Rs = _var7,
		call(Func,L,_var8),
		R = _var8,
		_var9 = [R|Rs],
		_var10 = _var9,
		_var2 = _var10
	)
	),
	_var11 = _var2,
	_var1 = _var11
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


std:exit(_var0) :- 
	halt,
	_var2 = tuple(  ),
	_var0 = _var2
	.


std:replace(List,Idx,NewVal,_var0) :- 
	std:size(List,_var1),
	ListSize = _var1,
	( (Idx<0;Idx>ListSize-1 -> (
		_var2 = write('Wrong Idx'),
		write('Wrong Idx'),
		std:exit(_var3),
		_var4 = tuple(  ),
		true);true)
	),
	( Idx=0 -> (
		( ListSize=0 -> (
			_var7 = [],
			_var8 = _var7,
			_var6 = _var8
		)
		;(
			_var9 = [L|Ls],
			List = _var9,
			_var10 = [NewVal|Ls],
			_var11 = _var10,
			_var6 = _var11
		)
		),
		_var12 = _var6,
		_var5 = _var12
	)
	;(
		_var13 = [L|Ls],
		List = _var13,
		_var14 is Idx-1,
		std:replace(Ls,_var14,NewVal,_var15),
		NewSubList = _var15,
		_var16 = [L|NewSubList],
		_var17 = _var16,
		_var5 = _var17
	)
	),
	_var18 = _var5,
	_var0 = _var18
	.


std:max(X,Y,_var0) :- 
	( X>=Y -> (
		_var1 = X
	)
	;(
		_var1 = Y
	)
	),
	_var2 = _var1,
	_var0 = _var2
	.


