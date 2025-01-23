
maxTree_lambda0(X,Y,_var2) :- 
	( X>=Y -> (
		_var3 = X
	)
	;(
		_var3 = Y
	)
	),
	_var4 = _var3,
	_var2 = _var4
	.
	

maxTree(Root,_var0) :- 
	_var1 = maxTree_lambda0,
	Max = _var1,
	_var6 = node(V,LeftChild,RightChild),
	Root = _var6,
	( LeftChild=nil,RightChild=nil -> (
		_var7 = write(V),
		write(V),
		_var8 = V,
		_var5 = _var8
	)
	;(
		( RightChild=nil -> (
			maxTree(LeftChild,_var10),
			call(Max,V,_var10,_var11),
			_var12 = _var11,
			_var9 = _var12
		)
		;(
			( LeftChild=nil -> (
				maxTree(RightChild,_var14),
				call(Max,V,_var14,_var15),
				_var16 = _var15,
				_var13 = _var16
			)
			;(
				maxTree(RightChild,_var17),
				maxTree(LeftChild,_var18),
				call(Max,_var17,_var18,_var19),
				MaxChild = _var19,
				call(Max,MaxChild,V,_var20),
				_var21 = _var20,
				_var13 = _var21
			)
			),
			_var9 = _var13
		)
		),
		_var5 = _var9
	)
	),
	_var22 = _var5,
	_var0 = _var22
	.


