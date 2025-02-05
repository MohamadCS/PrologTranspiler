# Introduction

`Prolog*` is a an extention to the Prolog programming language. It aims
to combine the functional features of Prolog, with modern features found
on other functional and oop programming languages.

`Prolog*` introduces many features, from complete functions, to
intuitive syntactic sugar.

# Compiler

## Usage

In order to compile a `Prolog*` we can use

``` {.bash language="bash"}
prolog -i my_file.txt 
```

this will compile `my_file.txt` to the prolog file `my_file.pl`.

We can add the following flags:

-   `-i –input` : To specify the input `Prolog*` file.

-   `-o –output` : Followed by the name of the prolog output file.

-   `–format`: Formats the output file.

-   `–no-semantics`: Disables the semantic checker.

::: note
*Note 1*. The output file must be interpreted using `swipl`, as its the
only interpreter tested for this project.
:::

# Expressions

Expressions in `Prolog*` are on of the following

-   Tuples.

-   Functions invocation.

-   Extended Prolog term.

-   Lambda.

-   Matching statment.

-   If-else statements.

We will expand on each one of them later.

## Extended Prolog Term

You can expect each prolog term to work as it is in regular prolog,
however `Prolog*` allows for new expressions that we defined above to be
used inside lists and predicate calls.

# Tuples

Tuples are simply lists of

-   Expressions

-   Bindings.

-   If statments (Without else).

Tuples follows the following rules

-   If the expression is followed by the seperator `,` then the
    expression is non-vanishing, that is its value is evaluated, and an
    entry in the final tuple.

-   If the expression is is followed by the seperator `;` then the
    expression is vanishing, that is its value is evaluated, and its not
    an entry in the final tuple.

-   If the last item is not followed by a seperator, then it is treated
    as a non-vanishing entry.

-   If the tuple is empty then its a vanishing entry regardless of its
    pair.

-   If the tuple entry is a direct predicate call, then its acts like in
    regular predicates, that is, if it evalutes to true then the
    execution of the function continues, otherwise it stopps. Regardless
    of the result, the entry would always be vanishing.

Tuples are compiled to the predicate `tuple(...)` when `...` are the
non-vanishing entries of the tuple.

## Examples

1.  `(1,2)`.

2.  `([3,4],Max(4,3),(1,2))`, evalutes to `tuple([3,4],Max(4,3),(1,2))`.

3.  `([3,4];Max(4,3),(1,2);)`, evalutes to `tuple(Max(4,3),(1,2))`.

::: note
*Note 2*. The number of entries in the tuple's predicate is determined
at compile time, for this reason, an if statement (Without else) can
only be a vanishing statement.
:::

# Binding

Binding is used for matching a variable, or a tuple of variables to
another expression. It does matching automatically, that is, if the
expression is an arithmitic term, it evalutes the term, and matches the
variable with the value of the term (like `is` operator) otherwise it
acts like `=` operator.

It have the intuitive syntax `Var <- Expr`.

## Tuple unpacking

For convinence, the language allows you to unpack the tuple and match
with named variables using the syntax `TupleOfVar <- Expr` when `Expr`
is evaluted to a tuple with the same size as the tuple of variables.

## Examples

-   `X <- 3 * Y;`, evalues `3*Y` then matches it with `X`.

-   `(X,Y) <- getMinMax(List);`, evalues `getMinMax(List)` to
    `tuple(Min,Max)` then matches `X` with `Min` and `Max` with `Y`.

-   `(X,Y) <- getMinMax(List);`, evalues `getMinMax(List)` to
    `tuple(Min,Max)` then matches `X` with `Min` and `Max` with `Y`.

# Conditionals

There are two types of conditionals if statements, and if-else
expressions.

If statements, evaluates the tuple in the if body if the condition is
true, while if-else evalutes one of them according to the condition.

The reason that `if` is a statement is that it must be always vanishing
to avoid ambiguity, since if the condition is false the if must not
evalute to a value, but a tuple entry must be known if its a vanishing
or non-vanishing at compile time.

In the otherhand, if-else provide us with a way to tell if the
expression is vanishing or not.

We can add some bindings at the head of the if condition in order for a
more compact syntax, this is an optional.

In order to allow for more readable nested if-else statements, if the
tuple has one entry only, and its non-vanishing, then we can remove the
parentheses.

## Syntax

``` {.prolog language="Prolog"}
if  optional(IfHead |)  Condition then 
        tuple
```

``` {.prolog language="Prolog"}
if optional(IfHead |) Condition then 
        tuple
    else 
        tuple
```

**Example**

::: enumerate
``` {.prolog language="Prolog"}
if Idx = 0 then (
        match List {
            [] => [],
            [L | Ls] => [NewVal | Ls]
        }
    ) else (
        List <- [L | Ls]; 
        [L | Replace(Ls,Idx - 1, NewVal)]
    )
```
:::

# Matching

Matches are expression that evalutes to a value and they have the
following syntax

``` {.prolog language="Prolog"}
match Expression {
        Exp1 => Result1,
        Exp2 => Result2,
        ...
        ExpN => ResultN,
        else => Result
    }
```

When `Resulti` are tuples or tuple entries.

Note that the `else` is optional.

# Functions

A functions, takes arguments, and returns a result.

A function result is a tuple of expressions, unless the tuple has one
entry, then its the expresssion that the tuple contains.

The functions name must be a valid variable's name that starts with a
capital letter. The function will compile to a regular `Prolog`
predicate but with the first letter being a small letter.

## Syntax

``` {.prolog language="Prolog"}
Func(Arg1, Arg2, ..., ArgN) :: tuple .
```

**Example**

``` {.prolog language="Prolog"}
Replace(List,Idx,NewVal) :: (

    if ListSize <- std:Size(List) | Idx < 0 ; Idx > ListSize - 1 then (
        write('Wrong Idx');
        Exit();
    );

    if Idx = 0 then (
        match List {
            [] => [],
            [L | Ls] => [NewVal | Ls]
        }
    ) else (
        List <- [L | Ls]; 
        [L | Replace(Ls,Idx - 1, NewVal)]
    )
)
.
```

## Arguments Alias

We can refer to the i-th argument of the function with the synatx `#i`.

`i` must be within the range of the function's arguments number.

We can also refer to the tuple that contains all the function's
arguments using `#`.

Example:

``` {.prolog language="Prolog"}
Max(X,Y) :: (if #1 >= #2 then #1 else #2).
```

``` {.prolog language="Prolog"}
F(X) :: (if tuple(X) = # then 1 else 0). // Evaluates to 1
```

## Lambdas

Lambdas are annonymos functions, and they are considered as expression,
meaning they can be binded to variables, returned from functions, passed
as an argument \...

They have the following syntax

``` {.prolog language="Prolog"}
(Arg1, Arg2, ..., ArgN) => (stmts)
```

**Example**

``` {.prolog language="Prolog"}
Foo(X,Y) :: (
        Max <- (X,Y) => (if #1 >= #2 then #1 else #2)
        Max(X,Y)
    )
    .
```

Note that `X,Y` are local to the lambdas.

# The Main Function

If the special function `Main/0` is defined, then prolog generates a
predicate with no arguments that defines this function, and it generates
a directive that runs the function automatically at the end of the file.

``` {.prolog language="Prolog"}
Main() :: (
        std:PrintLn("Hello");
    )
    .
```

In this example, if we load the file that contains this function, it
will print `Hello` without the need to call the predicate `main`.

# Modules

Modules are a abstraction of the current modules system of Prolog, they
contain functions or types, that can be marked as public using the
keyword `pub` for other files to use when the import the module

## Syntax

We can define a module using

``` {.prolog language="Prolog"}
module my_module {
        pub Foo() :: (...).
        pub type :: (...).
    }
```

And we can import the file the contains the module by listing the file
name without extenstion

``` {.prolog language="Prolog"}
import {
        'stdlib',
        'testlib',
        'heap',
    }
```

If we want to use a functions from a module we imported, we must declare
then module name before the function's

``` {.prolog language="Prolog"}

    import {
        'm1', // Has Foo/0 that prints 'hello'
        'm2'  // Has Foo/0 that prints 'world'
    }


    Main() :: (
        m1:Foo(); // Prints 'hello'
        m2:Foo(); // Prints 'world'
    )
    .
```

# Types

Types can be used for type checking, that is, inforcing the user to use
a pass a specific type to the function.

Function arguments and binding to a variable can declare the variable's
type using the syntax `Var : type`, if the type is a part of a module
then it must declare the module name before the type `Var : std:type`.

If the type is nullable then we pair it with `?`.

Example

``` {.prolog language="Prolog"}

    import {
        'stdlib'
    }

    Sort(List : std:list, Func) :: (
        ...
    )
    .

    Main() :: (
        Sort(1, _); // Type mismatch.
        Sort([], Min); // ok.

        X : std:list <- 1; // Type mismatch.
    )
    .
```

## Syntax

We can define a type by

``` {.prolog language="Prolog"}
type node :: (number , node?, node?).


    Foo(Node : node) :: (

    )
    .

    Main() :: (
        Foo(node(100,1,1)); // Type mismatch
        Foo(node(100,nil,nil)); // ok
        Foo(node(100,node(100,nil,nil),nil)); // ok
    )
    .
```

# Testing

`Prolog*` provides a builtin testing features.

A test function is defined like a regular function, but it has no name
and no arguments, instead, a describtion string must be provided.

## Syntax

``` {.prolog language="Prolog"}
test '<string of test desc>' tuple .
```

**Example**

``` {.prolog language="Prolog"}
test 'HeapSort test on random list' (

    RandomList <- std:ForEach( 
                              std:MakeList(100,0), 
                              (X) => (std:RandomNum(1,100))
    );

    testing:EXPECT_EQ(bin_heap:HeapSort(RandomList),
                      std:SortList(RandomList)
    );
)
.
```

If at least one test is defined, `Prolog*` will define a new function
called `RunTests/0`, which will run all tests, in the order of their
definitions.
