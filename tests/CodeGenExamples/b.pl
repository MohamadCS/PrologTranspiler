:- module(
	c,
	[

	]
)
.

:- use_module('a1.pl').
:- use_module('a2.pl').
:- use_module('std.pl').
main(_var1) :- 
	_var2 is 1,
	a:foo(_var2,_var3),
	std:printLn(_var3,_var4),
	_var5 is 2,
	b:foo(_var5,_var6),
	std:printLn(_var6,_var7),
	_var8 = tuple(  ),
	_var1 = _var8
	.


