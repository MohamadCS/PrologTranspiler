:- module(
	std,
	[
		vector/1,
		forEach/3,
		size/2,
		print/2,
		printLn/2,
		input/1,
		exit/1,
		replace/4,
		max/3,
		min/3,
		minMember/3,
		randomNum/3,
		makeList/3,
		sortList/2
	]
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
	_var2 = [  ],
	_var3 = [ L | Ls ],
	(
	List = _var2 -> 
	_var4 = [  ],
	_var1 = _var4
	;
	List = _var3 -> 
	call(Func,L,_var6),
	std:forEach(Ls,Func,_var7),
	_var5 = [ _var6 | _var7 ],
	_var1 = _var5
	;
	_var1 = nil
	),
	_var8 = _var1,
	_var0 = _var8
	.


size(List,_var0) :- 
	_var2 = [  ],
	_var3 = [ _ | Ls ],
	(
	List = _var2 -> 
	_var4 is 0,
	_var1 = _var4
	;
	List = _var3 -> 
	std:size(Ls,_var5),
	ListSize = _var5,
	_var6 is ListSize+1,
	_var7 = _var6,
	_var1 = _var7
	;
	_var1 = nil
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


input(_var0) :- 
	_var1 = read(X),
	read(X),
	_var2 = X,
	_var0 = _var2
	.


exit(_var0) :- 
	halt,
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
		_var7 = [  ],
		_var8 = [ L | Ls ],
		(
		List = _var7 -> 
		_var9 = [  ],
		_var6 = _var9
		;
		List = _var8 -> 
		_var10 = [ NewVal | Ls ],
		_var6 = _var10
		;
		_var6 = nil
		),
		_var11 = _var6,
		_var5 = _var11
	)
	;(
		_var12 = [ L | Ls ],
		List = _var12,
		_var14 is Idx-1,
		std:replace(Ls,_var14,NewVal,_var15),
		_var13 = [ L | _var15 ],
		_var16 = _var13,
		_var5 = _var16
	)
	),
	_var17 = _var5,
	_var0 = _var17
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


min(X,Y,_var0) :- 
	( Y>=X -> (
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
	_var2 = [  ],
	_var3 = [ X ],
	_var4 = [ X,Y ],
	_var5 = [ L | Ls ],
	(
	List = _var2 -> 
	_var1 = _var6
	;
	List = _var3 -> 
	_var1 = X
	;
	List = _var4 -> 
	call(MinFunc,X,Y,_var7),
	_var1 = _var7
	;
	List = _var5 -> 
	std:minMember(Ls,MinFunc,_var8),
	call(MinFunc,_var8,L,_var9),
	_var1 = _var9
	;
	_var1 = nil
	),
	_var10 = _var1,
	_var0 = _var10
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
	_var1 = length(List,Length),
	length(List,Length),
	_var2 = maplist(=(Value),List),
	maplist(=(Value),List),
	_var3 = List,
	_var0 = _var3
	.


sortList(List,_var0) :- 
	std:list(List),
	_var1 = msort(List,Result),
	msort(List,Result),
	_var2 = Result,
	_var0 = _var2
	.


list(X):-is_list(X).


