
:- use_module('stdlib.pl').
:- use_module('testlib.pl').
test3_lambda0(X,_var3) :- 
	_var4 is X*X,
	_var5 = _var4,
	_var3 = _var5
	.
	

test4_lambda0(X,_var3) :- 
	_var4 is X*X,
	_var5 = _var4,
	_var3 = _var5
	.
	

test0(_var0) :- 
	_var1 = [  ],
	std:size(_var1,_var2),
	_var3 is 0,
	testing:eXPECT_EQ(_var2,_var3,_var4),
	_var5 = tuple(  ),
	_var0 = _var5
	.


test1(_var0) :- 
	_var1 = [ 1 ],
	std:size(_var1,_var2),
	_var3 is 1,
	testing:eXPECT_EQ(_var2,_var3,_var4),
	_var5 = tuple(  ),
	_var0 = _var5
	.


test2(_var0) :- 
	_var1 = [  ],
	std:forEach(_var1,_var2,_var3),
	_var4 = [  ],
	testing:eXPECT_EQ(_var3,_var4,_var5),
	_var6 = tuple(  ),
	_var0 = _var6
	.


test3(_var0) :- 
	_var1 = [ 2 ],
	_var2 = test3_lambda0,
	std:forEach(_var1,_var2,_var6),
	_var7 = [ 4 ],
	testing:eXPECT_EQ(_var6,_var7,_var8),
	_var9 = tuple(  ),
	_var0 = _var9
	.


test4(_var0) :- 
	_var1 = [ 1,2,3 ],
	_var2 = test4_lambda0,
	std:forEach(_var1,_var2,_var6),
	_var7 = [ 1,4,9 ],
	testing:eXPECT_EQ(_var6,_var7,_var8),
	_var9 = tuple(  ),
	_var0 = _var9
	.


test5(_var0) :- 
	_var1 = [ 1 ],
	_var2 is 0,
	_var3 is 5,
	std:replace(_var1,_var2,_var3,_var4),
	_var5 = [ 5 ],
	testing:eXPECT_EQ(_var4,_var5,_var6),
	_var7 = tuple(  ),
	_var0 = _var7
	.


test6(_var0) :- 
	_var1 = [ 1,3,5 ],
	_var2 is 1,
	_var3 is 100,
	std:replace(_var1,_var2,_var3,_var4),
	_var5 = [ 1,100,5 ],
	testing:eXPECT_EQ(_var4,_var5,_var6),
	_var7 = tuple(  ),
	_var0 = _var7
	.


main :- 
	runTests(_var1),
	_var2 = tuple(  ),
	_var0 = _var2
	.


runTests(_var0) :- 
	_var1 = write("RUNNING TEST 0 : 'std:Size on empty list'\n"),
	write("RUNNING TEST 0 : 'std:Size on empty list'\n"),
	test0(_var2),
	_var3 = write("RUNNING TEST 1 : 'std:Size on list with one element'\n"),
	write("RUNNING TEST 1 : 'std:Size on list with one element'\n"),
	test1(_var4),
	_var5 = write("RUNNING TEST 2 : 'std:ForEach on empty list'\n"),
	write("RUNNING TEST 2 : 'std:ForEach on empty list'\n"),
	test2(_var6),
	_var7 = write("RUNNING TEST 3 : 'std:ForEach on list with one element'\n"),
	write("RUNNING TEST 3 : 'std:ForEach on list with one element'\n"),
	test3(_var8),
	_var9 = write("RUNNING TEST 4 : 'std:ForEach on list with multiple elements'\n"),
	write("RUNNING TEST 4 : 'std:ForEach on list with multiple elements'\n"),
	test4(_var10),
	_var11 = write("RUNNING TEST 5 : 'std:Replace on list with one element'\n"),
	write("RUNNING TEST 5 : 'std:Replace on list with one element'\n"),
	test5(_var12),
	_var13 = write("RUNNING TEST 6 : 'std:Replace on list with multiple elements'\n"),
	write("RUNNING TEST 6 : 'std:Replace on list with multiple elements'\n"),
	test6(_var14),
	_var15 = tuple(  ),
	_var0 = _var15
	.


:-main.


