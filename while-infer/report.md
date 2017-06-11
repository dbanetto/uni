% Assignment 4 - Inference & Generics - ENGR441
% David Barnett (300313764)

# What I have done

## Type inference

I added a `var` keyword to denote that a variable will have its
type inferred during the type checking phase.

There are two cases when inferring the type of the variable:

 * At declaration
 * Deferred, a future assignment will make it concrete

Below is examples of `while` code using `var`.
The inference at declaration is simple, it is simply taking the value
of the expression to the right and using that as its type.

Inferring the value when the declaration is deferred is slightly more
complex. In this implementation each unknown type is given the `inferred` type to
denote that it has not been figured out yet.
To make a variable go from `inferred` to a concrete type it must be assigned a
value. The first assignment to the variable will give the variable its type.
It becomes complex when that first assignment is on two different branches of a 
conditional.
It is possible to solve this for some types, such as records from both paths that intersect. 
Another solution would be to create a union type of the two branches.

```javascript
    var x = 10;
    // x is type int
    assert (x == 10);
```
> declaration example

```javascript
    var x;
    x = 10;
    // x is type int
    assert (x == 10);
```
> simple deferred example

```javascript
    var x;
    if (cond) {
        x = { f: true, a: 10 };
        // x is type { f: bool }
    } else {
        x = { f: true, c: "string" };
        // x is type { f: bool, c : String }
    }
    // x is type { f: bool }
    assert (x.f);
```
> complex deferred example, record subtype

```javascript
    var x;
    if (cond) {
        x = { f: true, c: 10, b : "string" };
        // x is type { f: bool, c : int, b : String  }
    } else {
        x = { f: true, c: "string" };
        // x is type { f: bool, c : String }
    }
    // x is type { f: bool }
    assert (x.f);
```
> complex deferred example, record intersection
