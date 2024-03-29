% Generating Simple Loop Invariants in Whiley
% David Barnett
% Supervisor: Lindsay Groves

# Motivation

 * Simplify an aspect of writing verified Whiley

 * Common loop invariants are generally simple and repetitive

 * Simple Invariants are obvious from inspection

# Background

 * Static methods
    + Uses techniques such as abstract interpretation
    + reliable but limited range of invariants

 * Dynamic methods
    + multiple attempts, larger range of invariants

 * Loop Invariant classification
     + Bounding Invariant - limits range of a variant

       > e.g. `i >= 0` when iterating an array

      + Essential Invariant - implies the post-condition

       > e.g. `i <= |xs|` when counting an array

# Design

 * Extend the Whiley Compiler
    + Adds another pass over the AST
 
 * Aim for loop invariants that are obvious from inspection
     + Should not introduce errors to previously compiling code
     + If an error occurs, generated invariants should not make for confusing error messages

 * Loop invariants are generated from the loop body and preceding code
    + Current state-of-the-art focus on weakening postconditions

 * Invariants based on Loop Patterns
    + Common loops have common invariants
    + indexing arrays, updating a counter

# Work so Far

 * A common search for all loops, collecting information
   about variable declarations and assignments.
     + Is given to all strategies as a starting point

 * Testing against 100+ tests from the Whiley Compiler's test suite with reduced
  loop invariants.

 * Implemented three invariant strategies
    + Starting Bound Invariant
    + Equal Array Length Invariant
    + Loop Condition Ageing Invariant


---

## Example

```javascript
function double(int[] items) -> (int[] r)
    ensures |r| == |items|
    ensures all { i in 0 .. |r| | r[i] == items[i] * 2 }:

    int i = 0
    int[] doubled = [0; |items|] // or just `items`

    while i < |doubled|
        // where i >= 0 - starting bound
        // where |items| == |doubled| - equal array length
        // where i <= |doubled| - loop condition ageing
        where all { j in 0 .. i |
                    doubled[j] == items[j] * 2 }:
        doubled[i] = items[i] * 2
        i = i + 1

    return doubled
```


# Evaluation

Metrics that have been identified to test against:

 * How many loop invariants would be removed over the test suite?
 * What invariant generators get the highest yield of reduced loop invariants?
 * How close to a programmer's invariants is the generated invariants?
 * Are all of the generated invariants useful?

# Future Work

Some options to focus on for the remaining time:

 * Implement new invariants
     + Identify & study more loop patterns
     + Look into universal quantifiers

 * Expanding the range of current invariants

----

## Questions
