:- module(
	std,
	[

		vector/1,
		forEach/3,
		size/2,
		exit/1,
		replace/4,
		max/3	]
)
.

vector(_var0) :- 
	_var0 = std:vector(_var1,_var2),
	list(_var1),
	number(_var2),
	true
	.
vector(_) :- write("Type mismatch"),fail.
forEach(List,Func,_var0) :- 
	std:size(List,_var2),
	Length = _var2,
	
	( Length=0 -> (
		_var3 = [],
		_var4 = _var3,
		_var1 = _var4
	)
	;(
		_var5 = [L|Ls],
		List = _var5,
		
		std:forEach(Ls,Func,_var6),
		Rs = _var6,
		
		call(Func,L,_var7),
		R = _var7,
		
		_var8 = [R|Rs],
		_var9 = _var8,
		_var1 = _var9
	)
	),
	_var10 = _var1,
	_var0 = _var10
	.


size(List,_var0) :- 
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


print(X,_var0) :- 
	_var1 = write(X),
	write(X),
	_var2 = tuple(  ),
	_var0 = _var2
	.


printLn(X,_var0) :- 
	std:print(X,_var1),
	_var2 = "\n",
	std:print(_var2,_var3),
	_var4 = tuple(  ),
	_var0 = _var4
	.


exit(_var0) :- 
	_var1 = halt,
	_var2 = tuple(  ),
	_var0 = _var2
	.


replace(List,Idx,NewVal,_var0) :- 
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


max(X,Y,_var0) :- 
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


list(X):-is_list(X).


