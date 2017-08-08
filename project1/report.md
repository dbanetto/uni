% SWEN304 - Project 1
% David Barnett - 300313764

# Question 1

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

## `Robberies`

```sql
CREATE TABLE Robberies (
    BankName text NOT NULL,
    City text NOT NULL,
    "Date" date NOT NULL,
    Amount money NOT NULL CHECK (Amount >= '0.0'::money),

    CONSTRAINT fk_robberies_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) ON UPDATE CASCADE
);
```

### Attributes

 * `BankName` is a text type as it is used as a foreign key to the `Banks` table, and is NON NULL because it is required.
 * `City` is a text type as it is used as a foreign key to the `Banks` table, and is NON NULL because it is required.
 * `Date` is a `date` type and NOT NULL as there are currently no requirement for a null value.
 * `Amount` is a `money` type, this is because this column is for the amount of money stolen.


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
On update of the referenced Bank the robberies would updated to changes
in the bank's name or city to keep.

## `Plans`

```sql
CREATE TABLE Plans (
    BankName text NOT NULL,
    City text NOT NULL,
    PlannedDate date,
    NoRobbers integer NOT NULL CHECK (NoRobbers >= 0),

    CONSTRAINT fk_plan_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) ON UPDATE CASCADE ON DELETE CASCADE
);
```

### Attributes

 * `BankName` is text and is a part of the foreign key, since a target must be known it is `NON NULL`
 * `City` is text and is part of the foreign key, since a target must be known it is `NON NULL`
 * `PlannedDate` is a date type, there is no constraint on null as the planned date might not be known.
 * `NoRobbers` is an integer of robbers, the constraint of having it being a positive number
   is added to ensure a valid plan and cannot be null as there must be someone or no one to
   be assigned to the plan.

### Keys

There is no suitable primary keys for this table.
This is due to being a valid business case that two robberies
could be planned for the same bank on the same day with the same amount
of people.

### Foreign Keys

This table as a foreign key relation to the `Banks` table via `BankName` and `City`.
This handles updates of banks by cascading the changes to its fields.
This handles deletes of banks by cascading as the details of a plan
to rob a non-existing bank are not important to keep.

## `Robbers`

```sql
CREATE TABLE Robbers (
    RobberId SERIAL PRIMARY KEY,
    Nickname text NOT NULL,
    Age integer,
    NoYears integer,

    CONSTRAINT non_neg_age CHECK (Age >= 0 AND NoYears >= 0),
    CONSTRAINT age_gt_noyears CHECK (Age >= NoYears)
);
```

### Attributes

* `RobberId` is a serial primary key ,
* `Nickname` is a text field as the name could be any text,  NOT NULL,
* `Age` is an integer, this has the additional constraint of being a positive number
   since it the robber must have a positive age. This can be null since the age of
   a robber could be unknown.
* `NoYears` is an integer this has the additional constraint of being a positive number
   since it the robber must have a positive years of experience. It can also not be greater
   than the age as they would be robbing longer then being alive.

### Keys

The primary key for this table is the `RobberId` instead of the `Nickname`.
This is because the `RobberId` will guarantee that it will be forever unique while
the nickname maybe unique for now but could be recycled later on.

### Foreign Keys

This table has no foreign keys.

## `Skills`

```sql
CREATE TABLE Skills (
    SkillId serial PRIMARY KEY,
    Description text NOT NULL UNIQUE
);
```

### Attributes

 * `SkillId` is a serial primary key so that the field can self-populate the ID
 * `Description` is a text field that has `NOT NULL` and `UNIQUE` constraints since
    the description of the skill should not have duplicates.

### Keys

The primary key is `SkillId` as the business rules outlines and allows for easier to be
referenced by other tables.

### Foreign Keys

`Skills` has no foreign keys but is referenced by other tables.

## `HasSkills`

```sql
CREATE TYPE Grades AS ENUM ('C-', 'C', 'C+', 'B-', 'B', 'B+', 'A-', 'A', 'A+');

CREATE TABLE HasSkills (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId),
    SkillId integer NOT NULL REFERENCES Skills(SkillId),
    Preference integer NOT NULL,
    Grade Grades NOT NULL,

    CONSTRAINT pref_positive CHECK (Preference > 0),
    CONSTRAINT unique_robber_skill UNIQUE (RobberId, SkillId),
    CONSTRAINT unique_robber_pref UNIQUE (RobberId, Preference)
);
```

### Attributes

 * `RobberId` is an integer for being used as a foreign key to `Skills` table. It has
    the `NON NULL` constraint as it is apart of a key.
 * `SkillId` is an integer for being used as a foreign key to `Skills` table. It has
    the `NON NULL` constraint as it is apart of a key.
 * `Preference` is an integer as the column is for ordering of skills, it has the constraint
    of `NON NULL` as it is a key. It also has the constraint of being greater than  zero to
    make ordering simpler.
 * `Grade` is a user-defined enum `Grades`, to restrict the possible values of the grade 
    and keep the value in a reasonable range. The constraint of `NOT NULL` is added
    as the robber will always have a level for their skill.

### Keys

This table has two keys and no primary key.
The combination of a `RobberId` and `SkillId` is to ensure that a robber will only
list a skill once.
The combination of a `RobberId` and `Preference` is to ensure that a robber cannot rank
two skills with the same preference to ensure a proper ordering.

### Foreign Keys

The `Skills` table has two foreign keys to `Robbers` and `Skills` via the columns `RobberId`
and `SkillId` respectfully.
The foreign key relation to `Robbers` is set to cascade on delete as the values 
in `HasSkills` lose value when the robber it is associated to is deleted.
The foreign key relation to `Skills` is set to <>.

## `HasAccounts`

```sql
CREATE TABLE HasAccounts (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId) ON DELETE CASCADE,
    BankName text NOT NULL,
    City text NOT NULL,

    CONSTRAINT fk_account_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City) ON DELETE CASCADE
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
