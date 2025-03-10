
:- use_module('heap.pl').
:- use_module('testlib.pl').
:- use_module('stdlib.pl').
test0_lambda0(X,_var5) :- 
	_var6 is 1,
	_var7 is 100,
	std:randomNum(_var6,_var7,_var8),
	_var9 = _var8,
	_var5 = _var9
	.
	

test0(_var0) :- 
	_var1 is 100,
	_var2 is 0,
	std:makeList(_var1,_var2,_var3),
	_var4 = test0_lambda0,
	std:forEach(_var3,_var4,_var10),
	RandomList = _var10,
	bin_heap:heapSort(RandomList,_var11),
	std:sortList(RandomList,_var12),
	testing:eXPECT_EQ(_var11,_var12,_var13),
	_var14 = tuple(  ),
	_var0 = _var14
	.


test1(_var0) :- 
	_var1 = [  ],
	bin_heap:heapSort(_var1,_var2),
	_var3 = [  ],
	testing:eXPECT_EQ(_var2,_var3,_var4),
	_var5 = tuple(  ),
	_var0 = _var5
	.


test2(_var0) :- 
	_var1 = [ 1 ],
	bin_heap:heapSort(_var1,_var2),
	_var3 = [ 1 ],
	testing:eXPECT_EQ(_var2,_var3,_var4),
	_var5 = tuple(  ),
	_var0 = _var5
	.


test3(_var0) :- 
	_var1 = [ 1,2,3 ],
	bin_heap:heapSort(_var1,_var2),
	_var3 = [ 1,2,3 ],
	testing:eXPECT_EQ(_var2,_var3,_var4),
	_var5 = tuple(  ),
	_var0 = _var5
	.


main :- 
	runTests(_var1),
	_var2 = tuple(  ),
	_var0 = _var2
	.


runTests(_var0) :- 
	_var1 = write("RUNNING TEST 0 : 'HeapSort test on random list'\n"),
	write("RUNNING TEST 0 : 'HeapSort test on random list'\n"),
	test0(_var2),
	_var3 = write("RUNNING TEST 1 : 'HeapSort test on empty list'\n"),
	write("RUNNING TEST 1 : 'HeapSort test on empty list'\n"),
	test1(_var4),
	_var5 = write("RUNNING TEST 2 : 'HeapSort test list with one element'\n"),
	write("RUNNING TEST 2 : 'HeapSort test list with one element'\n"),
	test2(_var6),
	_var7 = write("RUNNING TEST 3 : 'HeapSort on sorted list'\n"),
	write("RUNNING TEST 3 : 'HeapSort on sorted list'\n"),
	test3(_var8),
	_var9 = tuple(  ),
	_var0 = _var9
	.


:-main.


