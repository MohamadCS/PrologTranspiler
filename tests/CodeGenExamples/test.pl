
:- use_module('std.pl').
say(X,Y):-write(X),write(Y).


f(_var0) :- 
	_var1 = "hello",
	_var2 = _var1,
	_var0 = _var2
	.


g(_var0) :- 
	_var1 = "world",
	_var2 = _var1,
	_var0 = _var2
	.


test(_var0) :- 
	f(_var2),
	g(_var3),
	_var1 = say(_var2,_var3),
	say(_var2,_var3),
	true,
	_var5 = tuple(  ),
	_var0 = _var5
	.


