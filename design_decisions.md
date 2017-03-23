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

TODO

## Switch `default` case

Already implemented, missed it in the keywords list.

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

TODO: actually implement the const type & make the design decisions final

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

TODO: decide if it is worth checking if skip is the only thing in the block or just
another statement.
( former: edit parseStatementBlock to check for `skip` before parsing all statements )

## `switch` `case` with ranges and more

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

## union types
