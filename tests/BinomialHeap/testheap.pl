
:- use_module('heap.pl').
:- use_module('testing.pl').
:- use_module('std.pl').
testHeapSort1_lambda0(X,_var5) :- 
	_var6 is 1,
	_var7 is 100,
	std:randomNum(_var6,_var7,_var8),
	_var9 = _var8,
	_var5 = _var9
	.
	

testHeapSort1(_var0) :- 
	_var1 is 1000,
	_var2 is 0,
	std:makeList(_var1,_var2,_var3),
	List = _var3,
	_var4 = testHeapSort1_lambda0,
	std:forEach(List,_var4,_var10),
	RandomList = _var10,
	bin_heap:heapSort(RandomList,_var11),
	BinHeapSort = _var11,
	std:sortList(RandomList,_var12),
	StdSort = _var12,
	testing:eXPECT_EQ(BinHeapSort,StdSort,_var13),
	_var14 = tuple(  ),
	_var0 = _var14
	.


:- 
	testHeapSort1(_var1),
	_var2 = tuple(  ),
	_var0 = _var2
	.


