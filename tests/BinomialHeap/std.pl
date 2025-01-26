:- module(
	std,
	[

		vector/1,
		forEach/3,
		size/2,
		exit/1,
		replace/4,
		max/3,
		minMember/3,
		randomNum/3,
		makeList/3,
		sortList/2	]
)
.

vector(_var0) :- 
	_var0 = std:vector(_var1,_var2),
	list(_var1),
	number(_var2),
	!
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


minMember(List,MinFunc,_var0) :- 
	std:list(List),
	std:size(List,_var1),
	ListSize = _var1,
	
	( ListSize=0 -> (
		_var3 = nil,
		_var4 = _var3,
		_var2 = _var4
	)
	;(
		( ListSize=1 -> (
			_var6 = [X],
			List = _var6,
			
			_var7 = X,
			_var5 = _var7
		)
		;(
			( ListSize=2 -> (
				_var9 = [X,Y],
				List = _var9,
				
				call(MinFunc,X,Y,_var10),
				_var11 = _var10,
				_var8 = _var11
			)
			;(
				_var12 = [L|Ls],
				List = _var12,
				
				std:minMember(Ls,MinFunc,_var13),
				CurrMin = _var13,
				
				call(MinFunc,CurrMin,L,_var14),
				_var15 = _var14,
				_var8 = _var15
			)
			),
			_var5 = _var8
		)
		),
		_var2 = _var5
	)
	),
	_var16 = _var2,
	_var0 = _var16
	.


randomNum(Lower,Higher,_var0) :- 
	number(Lower),
	number(Higher),
	_var1 = random(Lower,Higher,X),
	random(Lower,Higher,X),
	_var2 = X,
	_var0 = _var2
	.


makeList(Length,Value,_var0) :- 
	number(Length),
	number(Value),
	_var1 = length(List,Length),
	length(List,Length),
	_var2 = maplist(=(Value),List),
	maplist(=(Value),List),
	_var3 = List,
	_var0 = _var3
	.


sortList(List,_var0) :- 
	std:list(List),
	_var1 = sort(List,Result),
	sort(List,Result),
	_var2 = Result,
	_var0 = _var2
	.


list(X):-is_list(X).


