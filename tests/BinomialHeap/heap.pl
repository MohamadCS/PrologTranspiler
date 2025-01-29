:- module(
	bin_heap,
	[

		heapSort/2	]
)
.

:- use_module('std.pl').
bin_heap:popMin_lambda0(Bt1,Bt2,_var2) :- 
	( Bt1=empty -> (
		_var4 = Bt2,
		_var3 = _var4
	)
	;(
		( Bt2=empty -> (
			_var6 = Bt1,
			_var5 = _var6
		)
		;(
			_var7 = bt(Value1,_),
			Bt1 = _var7,
			_var8 = bt(Value2,_),
			Bt2 = _var8,
			( Value1=<Value2 -> (
				_var10 = Bt1,
				_var9 = _var10
			)
			;(
				_var11 = Bt2,
				_var9 = _var11
			)
			),
			_var12 = _var9,
			_var5 = _var12
		)
		),
		_var3 = _var5
	)
	),
	_var13 = _var3,
	_var2 = _var13
	.
	

mergeBt(Bt1,Bt2,_var0) :- 
	_var1 = bt(Value1,List1),
	Bt1 = _var1,
	_var2 = bt(Value2,List2),
	Bt2 = _var2,
	( Value1>=Value2 -> (
		_var4 = bt(Value2,[Bt1|List2]),
		R = _var4,
		_var5 = R,
		_var3 = _var5
	)
	;(
		_var6 = bt(Value1,[Bt2|List1]),
		R = _var6,
		_var7 = R,
		_var3 = _var7
	)
	),
	_var8 = _var3,
	_var0 = _var8
	.


addBtAux(Bt,Heap,I,_var0) :- 
	std:list(Heap),
	number(I),
	_var1 = bt(Value,Children),
	Bt = _var1,
	std:size(Children,_var2),
	Order = _var2,
	std:size(Heap,_var3),
	HeapSize = _var3,
	( I=Order -> (
		( HeapSize=0 -> (
			_var6 = [Bt],
			Result = _var6,
			_var7 = tuple(  ),
			_var5 = _var7
		)
		;(
			_var9 = [CurrentBt|HeapTail],
			Heap = _var9,
			( CurrentBt=empty -> (
				_var10 = [Bt|HeapTail],
				Result = _var10,
				_var11 = tuple(  ),
				_var8 = _var11
			)
			;(
				bin_heap:mergeBt(CurrentBt,Bt,_var12),
				_var13 is I+1,
				bin_heap:addBtAux(_var12,HeapTail,_var13,_var14),
				NewHeapTail = _var14,
				_var15 = [empty|NewHeapTail],
				Result = _var15,
				_var16 = tuple(  ),
				_var8 = _var16
			)
			),
			_var5 = _var8
		)
		),
		_var17 = tuple(  ),
		_var4 = _var17
	)
	;(
		( HeapSize=0 -> (
			_var19 = "Heap size does not match the binomial heap",
			std:printLn(_var19,_var20),
			std:exit(_var21),
			_var22 = tuple(  ),
			_var18 = _var22
		)
		;(
			_var23 = [H|HeapTail],
			Heap = _var23,
			_var24 is I+1,
			bin_heap:addBtAux(Bt,HeapTail,_var24,_var25),
			NewHeapTail = _var25,
			_var26 = [H|NewHeapTail],
			Result = _var26,
			_var27 = tuple(  ),
			_var18 = _var27
		)
		),
		_var4 = _var18
	)
	),
	_var28 = Result,
	_var0 = _var28
	.


addBt(Bt,Heap,_var0) :- 
	std:list(Heap),
	_var1 is 0,
	bin_heap:addBtAux(Bt,Heap,_var1,_var2),
	_var3 = _var2,
	_var0 = _var3
	.


removeBt(Bt,Heap,_var0) :- 
	std:list(Heap),
	_var1 = bt(Value,Children),
	Bt = _var1,
	std:size(Children,_var2),
	Order = _var2,
	std:size(Heap,_var3),
	HeapSize = _var3,
	_var5 = [],
	_var6 = [Bt],
	_var7 = [Bt|HeapTail],
	_var8 = [H|HeapTail],
	(
	Heap = _var5 -> 
	_var9 = [],
	_var4 = _var9
	;
	Heap = _var6 -> 
	_var10 = [],
	_var4 = _var10
	;
	Heap = _var7 -> 
	_var11 = [empty|HeapTail],
	_var4 = _var11
	;
	Heap = _var8 -> 
	bin_heap:removeBt(Bt,HeapTail,_var12),
	NewHeapTail = _var12,
	_var13 = [H|NewHeapTail],
	_var14 = _var13,
	_var4 = _var14
	;
	_var4 = nil
	),
	_var15 = _var4,
	_var0 = _var15
	.


popMin(Heap,_var0) :- 
	std:list(Heap),
	_var1 = bin_heap:popMin_lambda0,
	std:minMember(Heap,_var1,_var14),
	MinBt = _var14,
	_var15 = bt(Value,List),
	MinBt = _var15,
	bin_heap:removeBt(MinBt,Heap,_var16),
	bin_heap:addList(List,_var16,_var17),
	ResultHeap = _var17,
	_var18 = tuple( MinBt,ResultHeap ),
	_var19 = _var18,
	_var0 = _var19
	.


add(Num,Heap,_var0) :- 
	number(Num),
	std:list(Heap),
	_var1 = bt(Num,[]),
	bin_heap:addBt(_var1,Heap,_var2),
	_var3 = _var2,
	_var0 = _var3
	.


addList(List,Heap,_var0) :- 
	std:size(List,_var1),
	ListSize = _var1,
	std:size(List,_var3),
	_var4 is 0,
	(
	_var3 = _var4 -> 
	_var2 = Heap
	;
	_var5 = [Bt|ListTail],
	List = _var5,
	bin_heap:addList(ListTail,Heap,_var6),
	bin_heap:addBt(Bt,_var6,_var7),
	_var8 = _var7,
	_var2 = _var8
	),
	_var9 = _var2,
	_var0 = _var9
	.


listToHeap(List,_var0) :- 
	std:list(List),
	std:size(List,_var2),
	_var3 is 0,
	(
	_var2 = _var3 -> 
	_var4 = [],
	_var1 = _var4
	;
	_var5 = [L|Ls],
	List = _var5,
	bin_heap:listToHeap(Ls,_var6),
	bin_heap:add(L,_var6,_var7),
	_var8 = _var7,
	_var1 = _var8
	),
	_var9 = _var1,
	_var0 = _var9
	.


heapToList(Heap,_var0) :- 
	std:list(Heap),
	std:size(Heap,_var1),
	HeapSize = _var1,
	( HeapSize=0 -> (
		_var3 = [],
		_var4 = _var3,
		_var2 = _var4
	)
	;(
		bin_heap:popMin(Heap,_var5),
		tuple(MinBt,NewHeap) = _var5,
		_var6 = bt(Value,_),
		MinBt = _var6,
		bin_heap:heapToList(NewHeap,_var7),
		Tail = _var7,
		_var8 = [Value|Tail],
		_var9 = _var8,
		_var2 = _var9
	)
	),
	_var10 = _var2,
	_var0 = _var10
	.


heapSort(List,_var0) :- 
	std:list(List),
	bin_heap:listToHeap(List,_var1),
	bin_heap:heapToList(_var1,_var2),
	_var3 = _var2,
	_var0 = _var3
	.


bt(_,[]).


bt(Value,[bt(ChildValue,ChildList)|RootListTail]):-length(RootListTail,K),length(ChildList,K),Value=<ChildValue,bt(Value,RootListTail).


