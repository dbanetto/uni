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
In particular, it removes an error with an inferred variable being defined with different types down
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

A result of using union types a programmer will need to be able to
distinguish what type is in the variable.
For example a test to see if a `int|bool` is an integer or boolean.


To achieve this I have added a `match` statement which is
similar to a `switch` statement expect it has cases based on types instead
of the value and each case is exclusive with no fall-through.

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
The semantics of the `match` statement is to branch into a `case` where the
given type matches
or is a sub-type of the runtime value of the variable.
Each `case` has a parameter to be defined inside the `case` block.
The type of the parameter must be a subtype of the expression used to match on.
The parameter defined for the `case` is an instantiated variable of a cloned
value of the given expression.
This allows for capturing the value of a method for error handing,
for example a simple echo program below.

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

\pagebreak

# Generics

I have looked into the implementations and lack thereof of generics in a 
selection of programming languages. These include: Java, Rust, Ada and Go.
Each of these languages have a different take on generics but follow the idea
of reusing code a type that can be inserted later. The black sheep in the selected
languages is Go since it strongly disagrees with use of generics but uses reflection
instead.

The common use of generics is for library authors to allow users to put in
user-defined types into data structures. A prime example is the Collections
API in Java which has a large range of different sets, vectors and queues that
can hold any type safely. The Collections API also shows the absence of generics
where the legacy collections only worked with objects and required more casting
than Hollywood to use.

Each section for a language will give an overview of the key points
of its generic system. A skeleton example of a generic container
to be sorted will be used to compare each language.

## Java

Generics in Java was added in Java 5 (2004) to allow a type or method to 
operate over a range of types with compile-time safety.
These come in two main forms, a generic class or a generic method (example below).
The main constraint on the Java generic system is that it only allows object types
to be used, thus you cannot have a generic over `int` or `boolean` but have to use
their boxed versions.

```java
class <T> Generic {
    <F> F map(T elem) {
        // ...
    }
}
```

The example above allows any types to replace `T` or `F` and the only 
known methods these types are the methods of `Object` (due to it being the root
type for any non-primitive). However, Java does allow for a restriction on the
type used by requiring the type to implement or extend a certain type or types.
For example to order a type it would need to implement the `Comparable`
interface, example below for Java.

```java
class <T: Comparable> Sortable {
    List<T> sort(List<T> items) {
        // ...
    }
}
```

The concept of generics in Java is contained only in the compiler.
This was a design decision to ensure backwards compatibility.
The result of this decision is that in the byte code the generic APIs
are transformed into a singe `Object`-based APIs with automatic
casting at the method boundaries to the concrete type.

## Ada

Ada is a language used commonly used for safety critical software and 
has had a concept of generics since Ada 1983, before any object orientated
features. Ada differs from Java and other modern language's generics
as it can have a range of generic parameters instead of only generics 
over types.
The generic parameters of Ada allow a user to provide a type or
a sub-program and even a value.
This allows a lot more uses than is available in generic systems
like Java's as you are not bound to use interfaces to restrict the
types or values.

```ada
generic
    type Element is private;
    with function "=" (left, right : Element) return Boolean is <>;
    with function "<" (left, right : Element) return Boolean is <>;
    Capacity : Natural;
package Sortable
    -- ..
end Sortable;
```

Ada implements generics by copying the AST of generic object and
replaces the generic parameters with the concrete values.
To provide syntax for this the user must initialise the generic,
example below.
In practice this method is unfriendly to the developer since
errors will only show at the initialisation site instead 
of the generic code.

```ada
package IntSortable is new Sortable(Element => Integer,
                                    Capacity => 10);
```

Ada's generic system is more verbose than other languages.
This is a result of Ada's design goal of being easy to read and
understand but at the cost of writing it.

## Rust

Rust is a relatively new language that focuses on systems
programming with the additions of modern programming language
advancements, e.g. functional style, traits and more.
Its use of generics are very similar to Java's system.
Currently Rust's generic system is only over types and can be
used in singular functions (seen below) and is extended to
traits (similar to Java's interfaces).


```rust
fn sort<T: PartialOrd> (items: &mut Vec<T>) {
    // ...
}
```

A key difference between Java and Rust is that generics are
available for any type. This includes primitive types to user defined
constructs. In Rust there is a distinction between data and functions
on the data, this is shown by the separation of a `struct` that
defines the data structure and an `impl` that implements functions
for that type (example below).

```rust
struct Point {
    x: i32, // 32-bit signed ingeters
    y: i32,
}

impl Point {
    // no constructors for a "class" in Rust
    fn new() -> Point {
        // struct construction
        Point{x: 0, y: 0}
    }
}
// e.g
let point = Point::new() // static calls use "::"
                         // instance with dot-notation
```

In Rust the `impl` block can implement can target a range
of types or a singular one (as shown above).
With Rust's generics a user can instruct that any type that
meets the restriction can use the defined implementation.
An example of this is below with the definition of a `Sortable`
trait, akin to a Java interface, and an `impl` for any type 
that implements the trait `PartialOrd`, a standard library 
trait for `<` and `>` operators. With the given `impl` 
any vector that contains a type that implements `PartialOrd`
can be sorted.

```rust
trait Sortable {
    fn sort(&mut self); // sorts in-place
}

impl <T: PartialOrd> Vec<T> for Sortable {
    fn sort(&mut self) {
        // ...
    }
}

// e.g
let items = Vec::new([3,1,2]);
items.sort();
```

Rust's use of generics makes it more useful to build up types by composition
than inheritance (which Rust does not currently have a notion of).
Compared to Ada's generics Rust's generics are easier to use and more
expressive with its ability to implement a function for entire class
of types.

The implementation of generics in Rust creates unique machine code
for each type uses the generic functions. This could create an
explosion of generated code but allows optimisations to be performed
for individual types, e.g. using SSE (optimised SIMD for floats)
instructions for float types.

## Go

The Go Language is also a relatively new language designed
for fast development of network applications.
The main features of Go are the use of non-native threads (allowing
thousands of threads at once with low cost) and a strong type system.
These come at the cost of requiring a runtime with a garbage collector
even though it is a compiled.
Go has no notation of generics but steers the user away with its
interfaces and powerful reflection.

```go
func sort(items []Comparable) []Comparable {
    // ...
}
```

The example above shows how a similar sort function would be
defined in Go. It uses an array of elements that implement the
interface `Comparable` and return a new array.

Though Go does not have generics like Ada, Java or Rust it could be
emulated for functions with the use of interfaces. 
The lack of generics shows where the guarantees of generics are helpful.
For example in the Go sort the user has to assume that the types returned
is of the same type that they put in, where with generics this not the case
and it is clearer the programmer to what is going on.
