
:- use_module('heap.pl').
:- use_module('testing.pl').
:- use_module('std.pl').
testHeapSort_lambda0(X,_var5) :- 
	_var6 is 1,
	_var7 is 100,
	std:randomNum(_var6,_var7,_var8),
	_var9 = _var8,
	_var5 = _var9
	.
	

testHeapSort(_var0) :- 
	_var1 is 30,
	_var2 is 0,
	std:makeList(_var1,_var2,_var3),
	_var4 = testHeapSort_lambda0,
	std:forEach(_var3,_var4,_var10),
	RandomList = _var10,
	bin_heap:heapSort(RandomList,_var11),
	std:sortList(RandomList,_var12),
	testing:eXPECT_EQ(_var11,_var12,_var13),
	_var14 = tuple(  ),
	_var0 = _var14
	.


:- 
	testHeapSort(_var1),
	_var2 = tuple(  ),
	_var0 = _var2
	.


