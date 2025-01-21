max(X,Y,_var0) :- 
	( X>=Y -> (
		_var1 = X
	)
	;(
		_var1 = Y
	)
	),
	_var2 = _var1,
	_var0 = _var2
	.


maxTree(Root,_var0) :- 
	_var1 = node(V,LeftChild,RightChild),
	Root = _var1,
	( LeftChild=nil,RightChild=nil -> (
		_var3 = V,
		_var2 = _var3
	)
	;(
		( RightChild=nil -> (
			maxTree(LeftChild,_var5),
			max(V,_var5,_var6),
			_var7 = _var6,
			_var4 = _var7
		)
		;(
			( LeftChild=nil -> (
				maxTree(RightChild,_var9),
				max(V,_var9,_var10),
				_var11 = _var10,
				_var8 = _var11
			)
			;(
				maxTree(RightChild,_var12),
				maxTree(LeftChild,_var13),
				max(_var12,_var13,_var14),
				MaxChild = _var14,
				max(MaxChild,V,_var15),
				_var16 = _var15,
				_var8 = _var16
			)
			),
			_var4 = _var8
		)
		),
		_var2 = _var4
	)
	),
	_var17 = _var2,
	_var0 = _var17
	.


