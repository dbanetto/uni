% Assignment 4 - Inference & Generics - ENGR441
% David Barnett (300313764)

# What is added to While

This section contains explanations and examples of the changes I have
made to the While compiler and language to add features like type inference and
others.

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

## Union Types with inference

Porting the union types from the first assignment has made for some interesting interactions with 
the type inference.
In particular it removes an error with an inferred variable being defined with different types down
two branches or more branches.
The response is to make the type a union of the two types.

```javascript
    var x;
    if (cond) {
        x = true;
    } else {
        x = "hello";
    }
    // x is type bool|string
```
> inferred types with unions

I added an extension when a union is of records, such as below, you can use common
fields from the records without the need to test which type of the union it is.
This allows some syntactic sugar to for easier use of union'd records.

```javascript
    var x;
    if (cond) {
        x = { f: true, a: 10 };
        // x is type { f: bool, a: int }
    } else {
        x = { f: true, c: "string" };
        // x is type { f: bool, c : String }
    }
    // x is type { f: bool, a: int }|{ f: bool, c: String }
    // x's type is functionally equalivant to { f: bool }
    assert (x.f);
```
> inferred types with records

```javascript
    var x;
    if (cond) {
        x = { f: true };
        // x is type { f: bool }
    } else {
        x = { f: "string" };
        // x is type { f: String}
    }
    // x is type { f: bool }|{ f: String }
    // x's type is functionally equalivant to { f: bool|String }
    assert (x.f);
```
> inferred types with records

The intersection with unions only include the field when all records in the union contain it otherwise
values will be create union since it would then include a `void` type to signify that a value does not exist.
This was not done to avoid revealing `void` in other contexts then the no-return of methods.

## Determining type at Runtime

As result of using union types a programmer will need to be able to distinguish what type is 
in the variable.
For example a test to see if a `int|bool` is an integer or a boolean.


To achieve this I have added a `match` statement which is similar to a `switch` statement expect
it has cases based on types instead of the value and each case is exclusive with no fall-through.

```
match ( expr ) {
    // where T is a sub-type of expr
    case ( T name ) {
        // statements
    }
    ...
}
```

```javascript
bool isTrue(int|bool input) {
    match(input) {
        case (int x) {
            return x != 0;
        }
        case (bool x) {
            return x;
        }
    }
}
```

Above is an example of the `match` syntax in action.
The semantics of the `match` statement is to branch into a `case` where the given type matches
or is a sub-type of the runtime value of the variable.
Each `case` has a parameter to be defined inside the `case` block.
The type of the parameter must be a subtype of the expression used to match on.
The parameter defined for the `case` is an instantiated variable of a cloned value of the given expression.
This allows for capturing the value of a method for error handing, for example a simple echo program below.

```javascript

// given a method: String|Err readLine()

String input;
match (readLine()) {
    case (Err e) {
        print("Error! " + e);
        return;
    }
    case (String val) {
        input = val;
    }
}
print(val);
```

With this style of error handling there is no need to throw exceptions and the programmer will know
and have to handle error cases. This is akin to the error handling in `rust` and other functional programming
languages.
