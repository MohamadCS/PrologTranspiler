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

If the tuple has one entry, and its vanishing then thery is no need
to wrap it with parentheses, which allows for more readable syntax
especially for the else if nested statements.

### Rules
- In if else statement, both tuples must have the same effective size.
- The function name is translated to the a predicate with the same name
but with the first letter in lower case, it has an additional variable which will hold the result of the function, the variable type of the return value of the function, if the function's effective size is 1,
otherwise its `tuple(E1,E2 ,... ,EN)` when `E1,...,EN` are the non-vanishing entries.
- If the function returns no value then its result is `tuple()`.
- If the tuple's effiective size is 0, then the tuple is a vanishing statements, wither its paired with `,` or not.

More examples can be found under `tests/CodeGenExamples`.

## Usage 
```bash
make 
./prolog -i <input_file> -o <output_file> 
./prolog --run_tests # to run the tests

```
