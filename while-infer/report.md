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

```javascript
    var x = 10;
    assert (x == 10);
```
> declaration example

```javascript
    var x;
    x = 10;
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
> complex deferred example

