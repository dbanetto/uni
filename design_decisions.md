# Assignment 1

Summary of changes to the while compiler and the decisions that were made.

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

Dead-code analysis works the same as a `while` loop with the following code being
invalid.

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
So far only `readline()` uses this, `print` could be transfered into this as well
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

However they differ in what they are allowed to be set to.
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

## `switch` `case` with ranges and more

There are two extensions to the `switch` statement added.

 * Allow `case` statements handle ranges of values.
 * Allow `case` statements to have alternatives cases.

```java
switch (n)
    case 0:
        // ...
        break;
    case 1 .. 10:
        // ...
        break;
    case -10 .. -1:
        // ...
        break;
    default:
        // ...
```

A method to implement these features would be to allow `case` to allow an array of the
`switch`'s type so that a range of values could be checked.
This would allow the user to use cases such as `case [1,4,6,7]:` for an integer.
The implementation of `range`s and `alternative`s would take advantage of this by 
just making a `range` be equivalent to stating an array of the same range and 
an `alternative` would just be summing the different arrays together to form an array
of all cases which the `case` will pass on.

However an implementation issue arose with this method, it is impossible distinguishable
between a natural array type, `int[]`, versus multiple cases of a single type, `int`.
To over come this issue all a `case` always is given an array of the type of the `switch`.

## union types

`UnionType ::= Type|Type`
