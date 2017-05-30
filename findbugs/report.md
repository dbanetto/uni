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
tool for java that combines model checking and various other types of checks built by NASA.
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

JPF allows for a large range of plugins to be enabled via the configuration.
This allows JPF is be modular with what properties to verify and other additional
features such as a GUI. The most common plugin used is `jpf-symbc` which provides
more symbolic execution features to JPF and is used for test generation and more.

## Examples

Using JPF is a battle to understand how all the levels of configuration work.
In general they are a cascading with directory level configuration
overrides global configuration.
The JPF JVM does not look to support all of Java 8 features which prevented
me from analysing my own concurrent code from NWEN303 Concurrency.
JPF also requires that the code to be analysed has a `main` method so
I could not run JPF over the Whiley Compiler by itself.

However, the binary snapshots of JPF is shipped with a range of working examples.
Below is an example that calculates $c = a / (b + a - 2)$ with each variable
being assigned a random number between 0 to 3 (inclusive).
This example demonstrates JPF's ability to rewind back to previous states and
testing a range of values by enumerating the random numbers.

```log
$ jpf +cg.enumerate_random=true Rand

====================================================== system under test
Rand.main()

====================================================== search started
computing c = a/(b+a - 2)..
a=0
  b=0       ,a=0
=>  c=0     , b=0, a=0
  b=1       ,a=0
=>  c=0     , b=1, a=0
  b=2       ,a=0

====================================================== error 1
gov.nasa.jpf.vm.NoUncaughtExceptionsProperty
java.lang.ArithmeticException: division by zero
	at Rand.main(Rand.java:34)


====================================================== snapshot #1
thread java.lang.Thread:{id:0,name:main,status:RUNNING,priority:5,
                         isDaemon:false,lockCount:0,suspendCount:0}
  call stack:
	at Rand.main(Rand.java:34)


====================================================== results
error #1: gov.nasa.jpf.vm.NoUncaughtExceptionsProperty
            "java.lang.ArithmeticException: division by zero  a..."

====================================================== statistics
elapsed time:       00:00:00
states:             new=4,visited=1,backtracked=2,end=2
search:             maxDepth=3,constraints=0
choice generators:  thread=1 (signal=0,lock=1,sharedRef=0,threadApi=0,reschedule=0), data=2
heap:               new=649,released=27,maxLive=615,gcCycles=4
instructions:       3354
max memory:         121MB
loaded code:        classes=65,methods=1366

====================================================== search finished
```

> Note: An additional flag needs to be passed to JPF so it knows it can expand the state space
by enumerating random numbers.

JPF also includes concurrent examples,
below is the output of checking their example Dining Philosopher which contains a deadlock.


```log
$ jpf DiningPhil

====================================================== system under test
DiningPhil.main()

====================================================== search started

====================================================== error 1
gov.nasa.jpf.vm.NotDeadlockedProperty
deadlock encountered:
  thread DiningPhil$Philosopher:{id:1,name:Thread-1,status:BLOCKED,priority:5,
                                 isDaemon:false,lockCount:0,suspendCount:0}

...

====================================================== snapshot #1
thread DiningPhil$Philosopher:{id:1,name:Thread-1,status:BLOCKED,priority:5,
                                 isDaemon:false,lockCount:0,suspendCount:0}
  owned locks:DiningPhil$Fork@162
  blocked on: DiningPhil$Fork@163
  call stack:
	at DiningPhil$Philosopher.run(DiningPhil.java:39)

...

====================================================== results
error #1: gov.nasa.jpf.vm.NotDeadlockedProperty
                                 "deadlock encountered:   thread DiningPhil$Philosop..."

====================================================== statistics
elapsed time:       00:00:03
states:             new=14470,visited=42568,backtracked=57010,end=36
search:             maxDepth=35,constraints=0
choice generators:  thread=14469 (signal=0,lock=9231,sharedRef=4,threadApi=6,
                                 reschedule=5228), data=0
heap:               new=412,released=132661,maxLive=392,gcCycles=57038
instructions:       362078
max memory:         416MB
loaded code:        classes=64,methods=1479

====================================================== search finished
```

> Note: Some of the output of the log as the removed as it repeats each error for
each thread in the example.

# Soot

[Soot](https://sable.github.io/soot/) is a static analyser for JVM bytecode that
was built originally to optimise byte code of Java programs but now is used to
analyse and instrument JVM based applications.
To achieve this Soot uses four intermediate representations (IR) to be able to
preform transformations easily. Each IR is a different abstraction over the
byte code and each one is tailored to suit a different kind of analysis.

## Capabilities of Soot

The capabilities are tied to its four intermediate languages, there are:

 * Baf
 * Jimple
 * Shimple
 * Grimp

Each IR can be transformed between each other and back to JVM byte code.

**Baf** is focused on being a stack-based representation and abstracts the
constant pool and reduces the type dependent instructions, such as `iadd`, `dadd` and etc., into
a single instruction.
This lends it to be useful for byte-code level analysis and optimisations.

**Jimple** is the main IR in Soot.
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

**Shimple** is Jimple with a twist, it uses static single assignment (SSA) and phi-nodes.
This guarantees that each local variable will has a single assignment which
simples a variety of analysis.
There is an issue with SSA, after a branch that assigns to the same variable in the source
it needs to represent the merge in SSA form.
Phi-nodes are the answer to this problem as they can determine which branch
was taken and thus can determine which value from the branch to use.
The strength of Shimple is in its ability to clearly show the control flow
of the program and also to analysis how variables change through it.

**Grimp** is similar to Jimple again but allows expressions to be trees instead of
linear.
It is regarded as the most human readable of the four IR's since it does not
need to create temporary variables such as `$t1` from the Jimple example.
To further increase readability Grimp merges the java object initialisation
instructions into one `new` statement.

Soot comes with some off-the-shell analyses.
These include null pointer, array bounds, and liveness analysis.

To accompany these four IR's Soot provides a framework to work with them,
such as customisable control flow graphs and flow sets.

[Soot's survivor Guide](http://www.brics.dk/SootGuide/sootsurvivorsguide.pdf)

## Examples

These tests were preformed with the most update to date stable release of Soot, 2.5.0
released in 2012.

There was some difficulties with running Soot.
There is a [bug in JDK 8](https://stackoverflow.com/questions/36963248) which makes
soot crash on startup, these examples were run with JDK 7.
Even with this fix, Soot never ran since Java could not find its `main` method.

I have attempted to build Soot from source which lead to another peculiar bug
for MacOS systems which can be resolved by a small [patch](https://github.com/Sable/soot/issues/686).
It does fix the immediate crash but since you have to build it, you cannot
recreate the same JAR configuration to get it to run again so you are stuck in an IDE.

After some help from Michael Person I did, what I assume to be, get the optimiser to run.

The command used is:
`java -jar soot-trunk.jar -v --app -cp .:$JAVA_HOME/jre/lib/rt.jar -process-dir $PWD -annot-nullpointer -annot-arraybounds`

# Comparison

FindBugs is by far the easiest to use out of the three tools.
From using the three tools it has displayed the most reliability
of using the tool but fails miserably when trying to compile it from
source code.
This is due to all documentation on the process is out-of-date and
inspecting the code itself reveals that the current setup is tailored to
the maintainers machines, not the general public.
The information gathered from FindBugs is quite useful to help improve the
quality of the code but it does not prove anything but the absence of its
bug patterns.

Java Path Finder (JPF) is the most powerful tool in terms of ability to prove
properties. The configuration that JPF allows can be used to enable a large range
of plugins that will check properties, this and JPF's ability to traverse every
execution path allows it to be an excellent tool for static and dynamic proving
of JVM programs.
What the configuration allows for is the strong point of JPF but actually
configuring it is a struggle and is one of its weak points for the end user.
The lack of support for Java 8 features is a real blow to what JPF can be
applied to but still has a good use case for proven java code.

Soot offers a framework to optimise your JVM programs and to preform some analysis.
Since it is designed as a framework Soot has more features geared towards a developer
extending Soot as a drop-in library.
However, the latest stable release does not work on an
update-to-date system and requires the user to downgrade their Java version to
(attempt to) use. Even with attempting to use the most latest version had no
result on trying to use it.
