:- module(
	a,
	[

	]
)
.

a:foo(X,_var1) :- 
	_var2 is 1,
	_var3 = _var2,
	_var1 = _var3
	.


