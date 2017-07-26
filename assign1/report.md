% SWEN304 - Assignment 1
% David Barnett (300313764)

# Question 1

## A)

A relation schema is a description of a relation defining the name of the relation,
the attributes of the relation, and constraints and domains of given attributes.

A relation is a finite set of tuples (or records) over a relation schema.

## B)

A candidate key is < >.
The properties of a candidate key are:

 * 

A foreign key is a <>.
The properties of a foreign key are:

 * 

## C)

A database is <>.
The main features of a database are

 * creating an abstraction of storing data and querying data for other applications
 * 

## D)

A database management system (DBMS) is <>.
The main tasks for a DBMS are:

 * 

# Question 2

<!--
    https://en.wikipedia.org/wiki/Superkey
    https://en.wikipedia.org/wiki/Candidate\_key
-->

## A)

 * `{ Employee }` in the relation there are two duplicates of `Tom` and `Minnie` with
 more valid duplicates likely to occur.
 * `{ DoB }` in the relation there are two duplicates of `22/01/1985` and with
 more valid duplicate to occur.
 * `{ JobTitle }` in the relation there are duplicates of `Waiter` with more valid
 duplicates likely to occur.
 * `{ DoB, JobTitle }` There are duplicates of two staff members having the
 same `dob` and `JobTitle` (`22/01/1985` and `Waiter`) and other valid duplicates
 are likely to occur.
* `{ Employee, DoB, StaffNo, JobTitle }` 

## B)

 * `{ StaffNo }` Yes,
 * `{ Employee, DoB }` No,
 * `{ Employee, StaffNo }` Yes,
 * `{ Employee, JobTitle }` No,
 * `{ DoB, StaffNo }` Yes,
 * `{ StaffNo, JobTitle }` Yes,
 * `{ Employee, DoB, StaffNo }` Yes,
 * `{ Employee, DoB, JobTitle }` No,
 * `{ Employee, StaffNo, JobTitle }` Yes,
 * `{ DoB, StaffNo, JobTitle }` Yes,

## C)

`{ StaffNo }` is a suitable candidate key to use as a primary key since it is the most minimal
candidate out of the possible options.

# Question 3

## A)

1. Yes, the primary key is unique for the given relation.

2. No, the primary key already exists and would no longer be unique if this tuple is added.

3. No, since the primary key cannot be *null* this tuple is not a valid insert.

4. ?Yes, since the primary key matches a valid tuple in the relation.

5. No, since the tuple is referenced by a `Pear` tuple in `PRODUCTS` relation, however
this is dependent on how this situation is set to be resolved: either rejecting the
query, setting the value to null or cascading the delete. In this case since the
foreign key is also a primary key for `PRODUCTS` setting the value to null is not an option.

## B)


1. Yes, the primary key of `PID` and `SID` are satisfied and the given `SID`
refers to a valid entry in the `SUPPLIER` relation to satisfy the foreign key constraint.
There is no constraint given that `Name` cannot be *null*.

2. Yes, the primary key matches on a single tuple.

3. No, the foreign key is invalid and does not refer to a valid entry in the `SUPPLIER` relation.

4. Yes, the tuple matches completely with a tuple in the relation.

5. Yes, the primary key is valid since `PID` is unique in the relation and `SID` references a
valid entry in the `SUPPLIER` relation.

# Question 4

## A)

`tourId` is a suitable candidate key, <>

## B)

### Customer

Has no suitable foreign key.

### Agent

Has no suitable foreign key.

### Booking

Has three suitable foreign keys,

 * `staffId`
 * `tourId`
 * `emailaddress`

### Tour

Has no suitable foreign key.

## C)

Depending if the `emailaddress` attribute is a foreign key to the `CUSTOMER` relation.

## D)

Additional bookings of tours with the same `emailaddress` (customer's account) 
and `dateFrom` (starting day of the tour) could not be made.
This would prevent a customer from making two bus tours on the same day, for example
one in the morning and another in the afternoon.

## E)

Cascading delete

## F)

Set foreign references to null.
