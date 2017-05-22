% Assignment 3 - Static Analysis Tools - ENGR441
% David Barnett (300313764)

This report will review multiple static analysis tools and some example usage of them.

# FindBugs

[FindBugs](http://findbugs.sourceforge.net/) is a static analysis tool for Java.
It identifies a large range of issues using what they call *bug patterns*.
The bug patterns not only check for common issues such as not implementing `hashCode()` if you
have overridden `equals` and also checks for common security vulnerabilities and concurrency problems.

FindBugs can be used in multiple ways from plugins for IDEs and build systems to standalone a GUI and CLI.
The source code is optional when checking for bugs as FindBugs uses the bytecode for static analysis.

## Capabilities of FindBugs

FindBugs has 8 distinct categories of bugs and 424 bug patterns implemented currently implemented.
The categories are:

* Bad practice
* Correctness
* Experimental
* Internationalization
* Malicious code vulnerability
* Multithreaded correctness
* Performance
* Security
* Dodgy code

To find these bug patterns FindBugs use a variety of methods to statically identify them.
A majority of the methods are based off control flow graphs built from the bytecode.

<!-- FIXME: not sure about this -->
From a practical point of view FindBugs is an advance code linter that is intended to
be used on complete code instead of in real-time like other common linters, [JSLint](http://jslint.com/) or [PEP8](https://pypi.python.org/pypi/pep8).

## Examples

For most of the examples I will be using what I have found while analysing the Whiley Compiler.

The following results use FindBugs version 3.0.1 scanning the entire source tree of Whiley Compiler.
FindBugs reports that is a possible 160 bugs, and 73 dodgy code in the Whiley Compiler. Through
inspecting some of these report some are valid others false positives.

One example is a check for cases falling through in switch statement, in all cases where this is
detected in the source code contains a comment explaining the behaviour.
Another interesting example is a check for initialised exceptions that are never thrown, in all of
these cases it is unsure if these are programmer error or not.

![Overview of bugs found](./examples/findbugs_bugs.png)

![Overview of dodgy code found](./examples/findbugs_dodgy.png)

# Java Path Finder

[Java Path Finder](http://babelfish.arc.nasa.gov/trac/jpf/) (JPF) is a static analysis
tool for java that combines model checking and various other types of checks.
At its core JPF is a java virtual machine that takes every execution path.
Each run of JPF has a configuration that encodes what properties to check in the
execution, this can range from correct usage of concurrent methods or unhandled exceptions.
The execution are explored with backtracking and state matching to check all paths.
The backtracking is used to go back to a branch point to try out a range of input values.
State matching is when JPF identifies that it has entered a similar state again and
determines that it does not need to re-explore that execution path again.

## Capabilities of JPF

JPF can work in three different environments.

 * `*.class` files that have no clue about JPF, referred to as 'JPF unaware'.
 * `*.java` files that have some annotations and additional information that helps JPF but does not rely on it, referred to as 'JPF aware'.
 * `*.java` files that are completely reliant on JPF and only uses other JPF reliant code, referred to as 'JPF enabled' java in the documentation.

Using JPF on `*.class` files allows you to model check for issues such as deadlocks.
The main benefit is the free usage of JPF without having to provide additional annotations
but this is at the cost of the efficiency that JPF will analyse the program.



The JPF virtual machine proves properties or finds defects defined from a
configuration file. 
The configuration determines which plugins are used for JPF for the project.
The java source code can also contain `@JPFConfig` annotations, but this will make
the code 'JPF aware' as it requires a dependency on JPF's API.


## Examples

# Soot

[Soot](https://sable.github.io/soot/) is a static analyser for JVM bytecode that
was built originally to optimise byte code of Java programs but now is used to
analyse and instrument JVM based applications.
To achieve this Soot uses four intermediate representations (IR) to be able to
preform transformations easily. Each IR is a different abstraction over the
byte code and each one is tailored to suit a different kind of analysis.

## Capabilities of Soot

The four intermediate languages are:

 * Baf
 * Jimple
 * Shimple
 * Grimp

Each IR can be transformed between each other and back to JVM byte code.

Baf is focused on being a stack-based representation and abstracts the 
constant pool and reduces the type dependent instructions, such as `iadd`, `dadd` and etc., into
a single instruction.
This lends it to be useful for byte-code level analysis and optimisations.

Jimple is the main IR in Soot.
The key features for Jimple are that it is untyped,
each statement is limited to addressing 3 variables
and expression are broken down to linear statements.
This means that an expressions such as `a = x + y + z` is broken
down to:

```java
    $t1 = x + y
    a = $t1 + z
```

This shows how Jimple breaks down larger expressions and how
the limit of 3 variables are used, one for assigning then the rest
for a binary operator or uni operator.


Shimple is Jimple with a twist, it uses static single assignment (SSA) and phi-nodes.
This guarantees that each local variable will has a single assignment which
simples a variety of analysis.
There is an issue with SSA, after a branch that assigns to the same variable in the source
it needs to represent the merge in SSA form.
Phi-nodes are the answer to this problem as they can determine which branch
was taken and thus can determine which value from the branch to use.
The strength of Shimple is in its ability to clearly show the control flow 
of the program and also to analysis how variables change through it.

Grimp is similar to Jimple again but allows expressions to be trees instead of
linear.
It is regarded as the most human readable of the four IR's since it does not 
need to create temporary variables such as `$t1` from the Jimple example.
To further increase readability Grimp merges the java object initialisation 
instructions into one `new` statement.

Soot comes with some off-the-shell analyses.
These include null pointer, array bounds, and liveness analysis.

To accompany these four IR's Soot provides a framework to work with them,
such as customisable control flow graphs and flow sets.

## Examples

# Comparison
