% JVM Compiler for While - ENGR441 - Assignment 2
% David Barnett - 300313764

The compiler is located in `src/whilelang/compiler/JvmCompiler.java` instead of the
standard `ClassFileWriter.java` which was the starting point for this assignment.
However, since I started before I got all the resources I ended up making it all from
scratch. Another difference from the basic starting point is that I used the most update
to date version of the `jasm` library (1.0.1, instead of 0.1.7 which is given).

The main difference between my version and what is given in the assignment is how the
current context is tracked.
In the assignment it is called `Context`, however I called mine `Environment`.
The major difference that I saw after the fact is that a single `Context` is to be used
throughout the entire translation phase,
in contrast `Environment` is a tree-like structure of environments
with only a relationship from the child node to the parent node and new environments are
closely related to new semantic scopes, such as in `if`'s or `while`'s.

# Part 1

Followed the assignment outline.

`assert` was already implemented in the original assignment but I got to implement it
myself and decided to use `AssertionError` over `RuntimeError` for Java-like authenticity.

# Part 2

Followed the assignment outline.

For implementing `break` & `continue` with my `Environment` it used the tree structure
to hold the valid labels for the current scope. Sometimes both were not used such as in
`switch` statements.

# Part 3

In this part I followed most of the hints expect for the hint to implement
the array generators with `Collections.nCopies()`. This was due to
the `List<>` that `nCopies` generates is a special implementation that is
read-only and then would fail when trying to edit a generated array.

To resolve this a similar method to array initialization is used and clones
the value for each slot in the array.

# Extensions

## print statement implemented

It was not required but was implemented for debugging purposes.

Implementation of `print` was interesting as it required to get the static
field `out` from `java.lang.System` and to support printing whatever type the expression
is.

## Compatibility with `java`

To allow for the class files generated from the compiler to be runnable using
a standard `java` execution the compiler has to ensure that a method with
the signature `public static main(String[])` existed.

This was accomplished by detecting if there was any method with this signature if not
the compiler would generate a method with the correct signature to call while's `void main()`
method.

Thus, any of the generated files can be run with `java`, e.g. `java While_Valid_1`
