:- module(
	hello,
	[

		node/1,
		isNode/2	]
)
.

node(_var0) :- 
	_var0 = node(_var1,_var2,_var3),
	number(_var1),
	(node(_var2) ; _var2 = nil),
	(node(_var3) ; _var3 = nil),
	true
	.
isNode(X,_var0) :- 
	std:node(X),
	_var1 = tuple(  ),
	_var0 = _var1
	.


