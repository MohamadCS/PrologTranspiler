# Prolog*

An extention to the Prolog langage, which adds support for functions. 


## Functions

```
Foo(Arg1,Arg2, ... , ArgN) :: (
    ...
) 
.
```

Such that `Arg1 ... ArgN` are variables, and the function body is a _tuple_.

A tuple is a list of expression, each expression can be a regular
Prolog term or a _binding_ which has the syntax `(Entry1 sep1 Entry2 sep2 ...)`.

Each entry is paired with `,` or `;`.

If the entry has `,` then its a non-vanishing entry. 
If the entry has `;` then its a vanishing entry. 

A vanishing entry is evaluted, but is not an entry in the result tuple,
and is not counted in _effective tuple size_.

If the last entry is not paired, then its non-vanishing.

## Invocation

We can invoke the function by `Func(E1,E2, ... , EN)`.

When `Ei` is one of the following:
- Invocation.
- Arithmetic expr.
- Variable.
- Predicate call.

```
Max(X,Y) % ok
Max(1,Y) % ok
Max(Max(3*5,X),Y) % ok
```

## Binding
Used to bind variables with function invocations, it works on arithemtic 
expr, lists, predicate and functions. This allows for a uniform binding 
syntax 

```
X <- 1 % ok
X <- Y * 100 % ok
X <- Max(X,Y) % ok
X <- Y = 10 % error
```

Note that `X <- Expr` assigns `Expr` to `X` then it evalutes to `X`  so
something like

```
(X <- Expr, 1)
```

Evaluates to 

```
(X,1)
```

So if you want a binding only, be sure to pair the binding with `;`.

## Argument aliases

We can refer to the `i`-th argument using `#i`, and we can refer
to the tuple that contains the function's arguments using `#`.

Example
```
Foo(X,Y,Z) :: (
    #1 <- 3; % Same as X <- 3;  
    T <- #; % Same as T <- tuple(X,Y,Z); 
    if List = [#3 | _] then (  % same as List = [Z | _]
        write(#3); // same as write(Z).
    );
)
.
```

Note that argument aliasing is done at preprocessing time.

## Conditionals

Conditionals are a tuple entry, and they have the following syntax

```
    if <Term> then tuple 
.
```

The result of the satement is the tuple if and only if `Term` is true, otherwise its the empty tuple.

We can use if else to evaluate one of the tuples

```
    if <Term> then
        tuple 
    else 
        tuple
.
```

If the tuple has one entry, and its non-vanishing then there is no need
to wrap it with parentheses, which allows for more readable syntax
especially for the else if nested statements.

## Lambdas

Lambdas are expressions, they can bind to variables, or passed to
functions.

```
X <- (X,Y) => (X + Y);
Y <- X(1,2)  % Y matches to 3

Sort(List, (X,Y) => (X >= Y));

```

## Modules

Modules defines a namespace for a set of functions, they are translated
to regular Prolog modules. However, there usage is much more intuitive.

To define a module
```cpp
module my_module {

pub Foo(X) :: ( 

)
.

F(X) :: (
)
.


}
```
Otherfiles can include that module

```

import {
    'my_module'
}

Bar(X) :: (
    my_module:Foo(X); % ok
    my_module:F(X); % Error, F is a private function.
)
.
```

If the function does not specify a namespace, then its namespace is automatically interpreted as the module's so it must be an enternal function.

Every function is private by default, unless it is declared as public using the keyword `pub`.



## Types

Syntax

```
type my_type :: (type1 ,type2 , ... , typeN).
```

such that `typei` is an atomic name, that can be chained with `?`, if it
does then the type is nullable (`typei` or `nil`).

We can declare the variable's type when binding, or in the function's arguments

```
import {
    'std'
}

pub Foo::(X : node) :: (
    Y : node <- X; // does not fail, since X is also a node 
    L : std:vector <- std:vector([1,2,3],3);// True  the type std:vector is defined in the our standard library

)
```




### Rules
- In if else statement, both tuples must have the same effective size.
- The function name is translated to the a predicate with the same name
but with the first letter in lower case, it has an additional variable which will hold the result of the function, the variable type of the return value of the function, if the function's effective size is 1,
otherwise its `tuple(E1,E2 ,... ,EN)` when `E1,...,EN` are the non-vanishing entries.
- If the function returns no value then its result is `tuple()`.
- If the tuple's effiective size is 0, then the tuple is a vanishing statements, wither its paired with `,` or not.
- Variables inside functions must not contain the prefix `_var`.

More examples can be found under `tests/CodeGenExamples`.

## Usage 
```bash
make 
./prolog -i <input_file> -o <output_file> 
./prolog --run_tests # to run the tests

```

### TODO:
- [x] # is a sytnax sugar that returns a tuple of the function's args.
- [x] #i the i-th arg.
- [x] Delete Variable after evalutating predicate. 
- [x] Write examples using the syntax sugar.
- [x] Add namespaces.
- [x] Add types and type checking.
- [ ] Change `::` to `<-`.
- [ ] Write a test program that build a binomaial tree of random size. 
- [ ] Modules can accept regular predicates
- [ ] Test delete min.
- [ ] Rewrite with syntax sugar.
- [ ] Add delayed/immediate defenition.





