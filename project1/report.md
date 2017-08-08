% SWEN304 - Project 1
% David Barnett - 300313764

# Question 1

```SQL

```

## `Banks`

```sql
CREATE TYPE SecurityLevel AS ENUM ('weak', 'good', 'very good', 'excellent');

CREATE TABLE Banks (
    BankName text NOT NULL,
    City text NOT NULL,
    NoAccounts integer DEFAULT 0,
    Security SecurityLevel NOT NULL DEFAULT 'weak',

    PRIMARY KEY (BankName, City),
    CONSTRAINT no_neg_accounts CHECK (NoAccounts >= 0) 
);
```

### Attributes

 * `BankName` is a `text` type since the field is free-form text, the constraint of `NOT NULL` 
    was added since it is a part of the primary key.
 * `City` is a `text` type since the field is free-form text, the constraint of `NOT NULL` 
    was added since it is a part of the primary key.
 * `NoAccounts` is an `integer` as it models a count, a constraint of `NoAccounts` must be equal to
    or greater than 0 as it is assumed that a bank cannot have less than 0 accounts. This attribute has
    a default of 0 since a new bank would start with 0 accounts open.
 * `Security` is a `SecurityLevel` enum as there is a finite number
   of levels of security. This attribute defaults to 'weak' as a
   new bank would be untrusted and untested so assumed to be weak.

### Keys

The primary key for `Banks` is `( BankName, City )` as the business rules guarantee that
this combination will always be unique.

### Foreign Keys

There are no foreign keys from this table.
However, there are multiple tables that have foreign keys to this table.

## `Robbers`

```sql
CREATE TABLE Robbers (

);
```

### Attributes

 * `BankName`
 * `City`
 * `Date`
 * `Amount` is a `money` type, this is because this column is for the amount of money stolen. It also ha

### Keys

There is no primary keys in this table.
As no attribute or combination of attributes are 
suitable as a key. This is due to all attributes can
have duplicates as there could be multiple robberies for
the same amount at the same bank on the same day.

### Foreign Keys

The table has a foreign key relation to `Banks` with the
attributes `BankName` & `City` to the same attributes of `Banks`.
On the deletion of the referenced Bank the robberies <>.

## `Skills`

```sql
CREATE TABLE Skills (

);
```

### Attributes

### Keys

### Foreign Keys

## `HasSkills`

```sql
CREATE TABLE HasSkills (

);
```

### Attributes

### Keys

### Foreign Keys

## `HasAccounts`

```sql
CREATE TABLE HasAccounts (

);
```

### Attributes

### Keys

### Foreign Keys

## `Accomplices`

```sql
CREATE TABLE Accomplices (

);
```

### Attributes

### Keys

### Foreign Keys

# Question 2

# Question 3

# Question 4

# Question 5
