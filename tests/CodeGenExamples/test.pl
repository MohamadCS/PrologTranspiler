
:- use_module('std.pl').
test0(_var0) :- 
	_var1 = tuple(  ),
	_var0 = _var1
	.


test1(_var0) :- 
	_var1 = tuple(  ),
	_var0 = _var1
	.


test2(_var0) :- 
	_var1 = tuple(  ),
	_var0 = _var1
	.


main :- 
	runTests(_var1),
	_var2 = tuple(  ),
	_var0 = _var2
	.


runTests(_var0) :- 
	_var1 = write("RUNNING TEST 0 : 'Hello'\n"),
	write("RUNNING TEST 0 : 'Hello'\n"),
	test0(_var2),
	_var3 = write("RUNNING TEST 1 : 'Another one'\n"),
	write("RUNNING TEST 1 : 'Another one'\n"),
	test1(_var4),
	_var5 = write("RUNNING TEST 2 : 'And another one'\n"),
	write("RUNNING TEST 2 : 'And another one'\n"),
	test2(_var6),
	_var7 = tuple(  ),
	_var0 = _var7
	.


:-main.


