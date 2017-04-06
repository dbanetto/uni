# Assignment 1

Summary of changes to the `while` compiler
and discussion of the decisions that were made.

## Do-While

Changes to the grammar:

```
DoWhileStmt ::= 'do' '{' Stmt* '}' 'while' '(' Expr ')' ';'
```

The syntax for the `do-while` block is the same as described in 2016's Assignment 1
of a `while` loop with one iteration guaranteed.
It also follows the rules for definite assignment of Java, a variable that is assigned
in a common control flow path in the `do-while` will be seen as assigned (example below). 

```java
int i;
do {
    i = 1;
} while( i == 1 );
```

Dead-code analysis and its interaction with `break` statements works the
same as a `while` loop with the following code being invalid.

```java
do {
    break;
    int i = 1;
} while( i == 1 );
```

## Input statement

To get a working input statement (`readline()`) the addition of
two other features were added, a standard library and a `syscall` statement.

The standard library was introduced so `readline()` was not an expression or a
statement but a method call.

The `syscall` statement is an internal use only for being able to interface
with operating system given functionality such as I/O.
So far only `readline()` uses this, `print` could be transferred into this as well
but has the additional problem of being able to take any expression an argument which
a method cannot give the same functionality without overloads of `print()` for every time.

The alternative way to implement an input statement would be to make it an
expression that would take no arguments and always return a string type.

## Switch `default` case

This feature was already implemented and I missed that during my read
of the compiler.

## `const` declaration

Changes to the grammar:

```
ConstVarDeclearationStmt ::= 'const' Type Ident '=' ConstExpr ';'

ConstVarExpr ::= Ident
```

The design of the `const` semantics is to follow what other languages do
to handle them. 
Java's closest idea to `const` is the `final` modifier, which makes the variable
be immutable after its first assignment.

Other languages use the keyword `const` but have different interpretations of it.
In `C` it is seen as an extension to the type to denote that it is immutable 
as opposed to something like `D` or `rust` where they are treated as
essentially aliases to a constant value and could be used in constant expressions.

In all of these languages they must be assigned a value at declaration unlike normal
variables.

```c
const int i = 0; // valid
const int n;
n = 1; // invalid
```

However, they differ in what they are allowed to be set to.
In `c` setting a `const` to an non-constant expression is allowed such as:

```c
int n = 10;
const int i = n;
```

But in `rust` the equivalent code is an error:

```rust
let n = 10;
const i: i32 = n; // err: attempt to use a non-constant value in a constant 
```

From researching the different styles of `const`'s in other languages the
final semantics implemented is:

 * a `const` can only be set to a constant value
 * a `const` must be initialized at declaration (parser ensures)
 * a `const` is usable in any constant expression (e.g. defining another `const` or a `switch` statement)

```java
int n = 10;
const int i = 100;
switch (n) {
    case i:
        break;
    default:
        break;
}
```

## `skip` statement

Changes to the grammar:

```
SkipStmt ::= 'skip' ';'
```

The `skip` statement is implemented as a no-op for a block.

The intended purpose is for to satisfy the constraint of having
to put something into a statement block such as an `if` or `switch`'s `case`.
This is useful to make all the branches to a large `if` statement and leave
some of them unimplemented for now, e.g.

```java
if (x == 0) {
    // ...
} else if (x > 0) {
    // TODO: implement later
    skip;
} else {
    // ..
}
```

It is a syntax error to have any other statements in the same block
as a `skip` e.g.

```
if (true) {
    skip;
    int i = 1; // error
}
```

A possible extension to the `skip` statement could be made if the
compiler has different modes for development and production compilation.
In development mode skips would emit warnings and errors in production
mode. This would be due to `skip` implies that not all functionality
has been implemented and there is still development to be done.

## `switch` `case` with ranges and alternatives

There are two extensions to the `switch` statement added.

 * Allow `case` statements handle ranges of values, e.g. `1..10`.
 * Allow `case` statements to have alternatives cases, e.g. `1 | 3 | 5`.

```java
switch (n) {
    case 0:
        // ...
        break;
    case 1 .. 10 | 30 .. 40:
        // ...
        break;
    case -10 .. -1:
        // ...
        break;
    default:
        // ...
}
```

A method to implement these features would be to allow `case` to allow an array of the
`switch`'s type so that a range of values could be checked.
This would allow the user to use cases such as `case [1,4,6,7]:` for an integer.
The implementation of `range`s and `alternative`s would take advantage of this by 
just making a `range` be equivalent to stating an array of the same range and 
an `alternative` would just be summing the different arrays together to form an array
of all cases which the `case` will pass on.

However, a severe implementation issue arose with this method, it is impossible distinguishable
between a natural array type, `int[]`, versus multiple cases of a single type, `int`.
This prevents the static check of distinct value for each `case` and being able to
correctly evaluate a `case` for an array type.
To get past this the idea that a switch case was just an array of candidates was dropped
by not making a `RangeExpr` a constant expression.

The final method used to determine a case with a Range and Alternatives was to make cases only take
an Alternative expression (defined below). This made a `case` have one or more alternatives
separated by a bar `|`. By not making `RangeExpr` a constant expression it allows a distinction 
between a natural `int[]` and a range of values `N..M` which solves the problems that arose
in the previous method but does limit the usage of `RangeExpr` but still keeps the expressiveness
of `case`'s but with only one syntax.

```
CaseStmt ::= AlternativeExpr
AlternativeExpr ::= ConstExpr | RangeExpr ( '|' AlternativeExpr )*

RangeExpr ::= ConstExpr '..' ConstExpr
```

Ranges are only implemented for integers and is validated in the type checker.
A consequence of using `ConstExpr` is that `const int`'s are acceptable in
ranges.

In the previous method a `case` could describe a set of candidates either in an array form or
with alternatives, e.g. `[1, 2, 3] == 1..3`, but arrays allowed for any sequence such as
`[1,2,5,6]` where ranges would need to be combined with alternatives `1..2 | 4..6`.

## union types

`UnionType ::= Type|Type`

This section is implemented as described in [Assignment 2](http://homepages.ecs.vuw.ac.nz/~lindsay/S430/assignment-2.pdf).

During the testing for casting I found that the width-subtype check for records
was not working correctly:

```java
type A is {int f, int g}
type B is {int f}

A a = { f: 1, b: 2}
B b = (B) a;

assert b.f == 1;
```

This was caused by the algorithm was checking that the subtype had more fields,
greater width, then the super type which is incorrect as `B` is a subset of
values of `A` and thus the subtype.

An extension to this would be to add runtime type checks or flow typing
to ensure that the cast is valid.
This could come in the form of an expression, such as `Expr 'is' Type`, returning a boolean
that will check the type of the expression at runtime.
However, since all type checks are currently static only to implement this 
would  most likely require needing to wrap all objects with some metadata
about what their types.
