% SWEN304 - Assignment 1
% David Barnett (300313764)

# Question 1

## A)

A relation schema is a description of a relation defining the name of the relation,
the attributes of the relation, and constraints and domains of given attributes.

A relation is a finite set of tuples (or records) over a relation schema.

## B)

A candidate key is a minimal super key for a given relation.
The properties of a candidate key are:

 * The relation does not have two distinct tuples with the same values for the candidate key attributes. 
 * There is no proper subset of candidate key such that it is minimal.  

A foreign key is a means to relate two relations together via a shared attribute.
The relation is one way from the 'relator' to the 'related'.
The properties of a foreign key are:

 * The attribute of the 'related' is a sub-set of the primary of that relation.
 * The shared attribute must be consistent of the domain and range of the attribute,
    such that the 'relator' can represent any valid value of the 'related'.

## C)

A database is a collection of related data.
The main features of a database are:

 * Creating an abstraction of storing data and querying data for other applications.
 * Represents an aspect of the world.
 * Well structured.
 * Reflects the current state of the world.
 * Has users.
 * Stores the data in a persistent manner.
 * Is managed by a database management system.

## D)

A database management system (DBMS) is general purpose software that
allows operations of the managed database.
The main tasks for a DBMS are:

 * defining structure
 * constructing data
 * querying, updating and removing data
 * preserve consistency by ensuring constraints on data are met
 * protect from misuse, e.g. Unauthenticated user, malformed queries, etc.
 * recovering from failure, such as backups
 * allow for concurrent use from many users.

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
 * `{ DoB, StaffNo }` has a proper subset of `{ StaffNo }` 
 * `{ Employee, StaffNo }` has a proper subset of `{ StaffNo }` 
 * `{ StaffNo, JobTitle }` has a proper subset of `{ StaffNo }`
 * `{ Employee, DoB, StaffNo }` has a proper subset of `{ StaffNo }`
 * `{ Employee, StaffNo, JobTitle }` has a proper subset of `{ StaffNo }`
 * `{ DoB, StaffNo, JobTitle }` has a proper subset of `{ StaffNo }`
 * `{ Employee, DoB, StaffNo, JobTitle }` has a proper subset of `{ StaffNo }` 
 * `{ Employee, DoB, JobTitle }` has a proper subset of `{ Employee, DoB }` and `{ Employee, JobTitle }`
 
## B)

 * `{ StaffNo }` Yes, it does not seem to be likely that there will ever be a
 duplicate value of `StaffNo` in the relation.
 * `{ Employee, DoB }` No, it is feasible that a duplicates of these attributes
 could exist in the relation given that subsets of the attributes already have duplicates.
 * `{ Employee, JobTitle }` No, it is feasible that a duplicates of these attributes could exist in the relation given additional data given that subsets of the attributes already have duplicates.


## C)

`{ StaffNo }` is a suitable candidate key to use as a primary key since it is the most minimal
candidate out of the possible options and all attributes in the key set have shown
that they are unique unlike any other attribute of the relation.

# Question 3

## A)

1. Yes, the primary key is unique for the given relation.

2. No, the primary key already exists and would no longer be unique if this tuple is added.

3. No, since the primary key cannot be *null* this tuple is not a valid insert.

4. ?Yes, since the primary key matches a valid tuple in the relation.

4. ?No, even though the primary key matches on a tuple in the relation, but the rest
of values do not match to the tuple.

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

There are no suitable candidate key for the `TOUR` relation.
This is due to both `tourId` and `destination` attributes would 
not meet the unique property required since multiple tours could
go to a single destination and equally a single tour could go to
multiple destinations.

A candidate key of `{tourId, destination}` is suitable as together they
would be unique assuming that a tour would not revisit the same destination
twice.

## B)

### Customer

Has no suitable foreign key as the relation is standalone information about
the customer.

### Agent

Has no suitable foreign key as the relation is standalone about the travel agent.
It may look like `name` could be a foreign key to `CUSTOMER` since an agent
could also be a customer but this is not always the case and making it a foreign
key would force agents to also be customers, however this is redundant information
in the case of a person is both a customer and an agent.

### Booking

Has three suitable foreign keys,

 * `staffId` to the `AGENT` relation
 * `tourId` to the `TOUR` relation given that `tourId` is apart of primary key
 of `TOUR`.
 * `emailaddress` to the `CUSTOMER` relation

These are because a booking requires these fields to be valid to be a valid
booking.

### Tour

Has no suitable foreign key as the relation is standalone information about
the destinations of a tour.

## C)

Depending if the `emailaddress` attribute is a foreign key to the `CUSTOMER` relation.
If the `emailaddress` in the `BOOKING` relation is a foreign key, then the booking
with an email address that is not associated with any client would be rejected since
the foreign key must refer to a valid tuple in the `CUSTOMER` relation with the same
value.
If the `emailaddress` in the `BOOKING` relation is not a foreign key, then the
booking of an unknown email address is a valid insertion.

## D)

Additional bookings of tours with the same `emailaddress` (customer's account) 
and `dateFrom` (starting day of the tour) could not be made.
This would prevent a customer from making two bus tours on the same day, for example
one in the morning and another in the afternoon.

## E)

To ensure that all bookings made by the customer is deleted with the deletion of
the customer from the `CUSTOMER` relation would be most effectively handled by
making the `emailaddress` attribute in `BOOKING` a foreign to `CUSTOMER` and
configure the constraint to handle deletes as cascading deletes.
This makes all tuples in `BOOKING` that has a foreign key pointing to the tuple in
`CUSTOMER` being removed to also be removed as well.

## F)

To ensure that all bookings made by an agent are not deleted when the agent
is deleted would be most effectively handled by making a foreign key relation
between `staffId` in the `BOOKING` relation a foreign key to `staffId` in the
`AGENT` relation.
This foreign key can be configured to be set to null when the tuple that the
booking tuple refers to is deleted. This ensures that all bookings are kept
upon deletion of the agent and also *null* values would represent the agent 
has left.
