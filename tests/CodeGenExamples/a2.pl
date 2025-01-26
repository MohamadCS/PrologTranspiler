:- module(
	b,
	[

	]
)
.

b:foo(X,_var1) :- 
	_var2 is 2,
	_var3 = _var2,
	_var1 = _var3
	.


