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

    CONSTRAINT uniq_robberies UNIQUE (BankName, City, "Date"),

    CONSTRAINT fk_robberies_bank
    FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON UPDATE CASCADE ON DELETE CASCADE
);
```

### Attributes

 * `BankName` is a text type as it is used as a foreign key to the `Banks` table, and is NON NULL because it is required.
 * `City` is a text type as it is used as a foreign key to the `Banks` table, and is NON NULL because it is required.
 * `Date` is a `date` type and NOT NULL as there are currently no requirement for a null value.
 * `Amount` is a `money` type, this is because this column is for the amount of money stolen.


### Keys

The key for the table is `{BankName, City, Date}` because it
is the minimum candidate key for the table. In the case
of a duplicate the `Amount` stolen record should just be a sum
of all the robberies that day but it is assumed the bank will
close for the rest of the day after a robbery.

### Foreign Keys

The table has a foreign key relation to `Banks` with the
attributes `BankName` & `City` to the same attributes of `Banks`.
On the deletion of the referenced Bank the robberies will also be deleted via cascade.
On update of the referenced Bank the robberies would updated to changes
in the bank's name or city to keep.

## `Plans`

```sql
CREATE TABLE Plans (
    BankName text NOT NULL,
    City text NOT NULL,
    PlannedDate date NOT NULL,
    NoRobbers integer NOT NULL CHECK (NoRobbers >= 0),

    CONSTRAINT fk_plan_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON UPDATE CASCADE ON DELETE CASCADE
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
    RobberId integer NOT NULL REFERENCES Robbers(RobberId)
        ON DELETE CASCADE ON UPDATE CASCADE,
    SkillId integer NOT NULL REFERENCES Skills(SkillId)
        ON DELETE CASCADE ON UPDATE CASCADE,
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
The foreign keys are set to cascade on delete and update so
that the `HasSkills` table reflects what is currently in the
database without nulls.

## `HasAccounts`

```sql
CREATE TABLE HasAccounts (
    RobberId integer NOT NULL REFERENCES Robbers(RobberId)
        ON DELETE CASCADE ON UPDATE CASCADE,
    BankName text NOT NULL,
    City text NOT NULL,

    CONSTRAINT one_robber_acc UNIQUE (RobberId, BankName, City),
    CONSTRAINT fk_account_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON DELETE CASCADE ON UPDATE CASCADE
);
```

### Attributes

 * `RobberId` is an integer field as it is a foreign key to `Robbers`, and is set to be non-null
    as it is a part of key of the table.
 * `BankName` is a text field as it is a foreign key to `Banks`, and is set to be non-null
    as it is a part of key of the table.
 * `City` is a text field as it is a foreign key to `Banks`, and is set to be non-null
    as it is a part of key of the table.


### Keys

The key for this table is all the columns as the purpose of the table is a binary question of
if a robber has an account(s) at a bank or not. Thus having duplicate entries is redundant.

### Foreign Keys

`RobberId` is a foreign key to the `Robbers` table. This has been configured
cascade on deletion or an update of the referenced robber it will be deleted or updated
respectfully.
This is the same for the `BankName` and `City` foreign key to `Banks`.

## `Accomplices`

```sql
CREATE TABLE Accomplices (
    RobberId integer REFERENCES Robbers(RobberId)
        ON UPDATE CASCADE ON DELETE CASCADE,
    BankName text NOT NULL,
    City text NOT NULL,
    RobberyDate date NOT NULL,
    "Share" money NOT NULL,

    CONSTRAINT non_neg_share CHECK ("Share" >= '0'::money),
    CONSTRAINT uniq_accomplices UNIQUE
        (RobberId, BankName, City, RobberyDate),
    CONSTRAINT fk_accomplice_bank FOREIGN KEY (BankName, City)
    REFERENCES Banks(BankName, City)
        ON UPDATE CASCADE ON DELETE CASCADE,
    CONSTRAINT fk_accomplice_robbery FOREIGN KEY (BankName, City, RobberyDate)
    REFERENCES Robberies(BankName, City, "Date")
        ON UPDATE CASCADE ON DELETE CASCADE
);
```

### Attributes

 * `RobberId` is an integer field and is a part of the foreign key to
    the `Robbers` table. It is non-null as it is required for the
    data to make sense and is a part of the key.
 * `BankName` is a text field and is a part of the foreign key to `Banks`.
    It is non-null as it is required for the data to make sense and is a part of the key.
 * `City` is a text field and is a part of the foreign key to `Banks`.
    It is non-null as it is required for the data to make sense and is a part of the key.
 * `RobberyDate` is a date field, and has the constraint of non-null
    as there is always a known date and is a part of the key.
 * `Share` is a money field as the data represented is money, has
    the added constraints of being a positive number and not null.

### Keys

The key for this table is `{RobberId, BankName, City, RobberyDate}`
because this combination is the minimum unique key.
In the case of the same robber is an accomplice to a robbery
on the same date at the same bank the `Share` value would be
the sum of the these two robberies but it is assumed the bank will
close for the rest of the day after a robbery.

### Foreign Keys

This table has three foreign keys to `Banks` and `Robbers` and
`Robberies`.
Each table's primary keys are included in this table to
reference these tables.
Both foreign relations use a cascade technique for updates
and deletions of their foreign keys.
This is so the Accomplices table can follow the changes of
the tables it is dependent on.

# Question 2

## Part 1

For the `Banks`, `Robberies`, `Plans` and `Robbers` tables
a simple `\COPY` command into the tables was sufficient to
load the data.

```sql
\COPY Banks FROM data/banks_17.data;
\COPY Robberies FROM data/robberies_17.data;
\COPY Plans FROM data/plans_17.data;
\COPY Robbers(Nickname, Age, NoYears) FROM data/robbers_17.data;
```

To load data into the `Skills` and `HasSkills` table the
use of a temporary table, `SkillsRaw`, is used
because it requires some manipulation.
For the `Skills` table this distinct descriptions of skills
are extracted from the `SkillsRaw` table to ensure there
are no duplicates.
For `HasSkills` a natural join between `SkillsRaw`, `Robbers` and
`Skills` so that the related robber skills are paired with
their `SkillIds`.

```sql
CREATE TABLE SkillsRaw (
    Nickname text NOT NULL,
    Description text NOT NULL,
    Preference integer NOT NULL,
    Grade text NOT NULL
);

\COPY SkillsRaw FROM data/hasskills_17.data

INSERT INTO Skills (Description)
    (SELECT DISTINCT (Description) FROM SkillsRaw);

INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    (SELECT RobberId, SkillId, Preference, TRIM(Grade)::Grades
        FROM skillsraw NATURAL JOIN robbers NATURAL JOIN skills);

DROP TABLE SkillsRaw;
```

`HasAccounts` also requires the use of a temporary table to
allow for a natural join between robbers to pair the nicknames
used to robber ids.
This has the assumption that for the data given that
the nicknames are unique.

```sql
CREATE TABLE HasAccountsRaw (
    Nickname text NOT NULL,
    BankName text NOT NULL,
    City text NOT NULL
);

\COPY HasAccountsRaw FROM data/hasaccounts_17.data

INSERT INTO HasAccounts (RobberId, BankName, City)
    (SELECT RobberId, BankName, City
        FROM HasAccountsRaw NATURAL JOIN Robbers);

DROP TABLE HasAccountsRaw;
```

`Accomplices` also uses a temporary table to pair up
nicknames of robbers with their robber ids.
This has the assumption that for the data given that
the nicknames are unique.

```sql
CREATE TABLE AccomplicesRaw (
    Nickname text NOT NULL,
    BankName text NOT NULL,
    City text NOT NULL,
    RobberyDate date NOT NULL,
    "Share" money NOT NULL
);

\COPY AccomplicesRaw FROM data/accomplices_17.data

INSERT INTO Accomplices (RobberId, BankName, City, RobberyDate, "Share")
    (SELECT RobberId, BankName, City, RobberyDate, "Share" FROM
    AccomplicesRaw NATURAL JOIN Robbers);

DROP TABLE AccomplicesRaw;
```

## Part 2

A partial order of the data being loaded was enforced by the
dependencies between tables.
This made certain key tables of `Banks`, `Robbers` and `Skills` have to
be loaded before other tables that reference it, such as
`Accomplices` or `HasSkills`

# Question 3

## 1.

### A)

```sql
INSERT INTO Banks (BankName, City, NoAccounts, Security)
    VALUES ('Loanshark Bank', 'Evanston', 100, 'very good'::SecurityLevel);
```

Error produced:

`ERROR:  duplicate key value violates unique constraint "banks_pkey"
DETAIL:  Key (bankname, city)=(Loanshark Bank, Evanston) already exists.`

### B)

```sql
INSERT INTO Banks (BankName, City, NoAccounts, Security)
    VALUES ('Loanshark Bank', 'Evanston', -5, 'excellent'::SecurityLevel);
```

Error produced:

`ERROR:  new row for relation "banks" violates check constraint "no_neg_accounts"
DETAIL:  Failing row contains (Loanshark Bank, Evanston, -5, excellent).`

### C)

```sql
INSERT INTO Banks (BankName, City, NoAccounts, Security)
    VALUES ('Loanshark Bank', 'Evanston', 100, 'poor'::SecurityLevel);
```

Error produced:

`ERROR: invalid input value for enum securitylevel: "poor"
 VALUES ('Loanshark Bank', 'Evanston', 100, 'poor'::Secur...`

This is caused by using my defined type `SecurityLevel` to enforce
an acceptable list of security levels.

## 2.

### A)

```sql
INSERT INTO Skills (SkillId, Description)
    VALUES (20, 'Guarding');
```

Error produced:

`ERROR:  duplicate key value violates unique constraint "skills_description_key"
DETAIL:  Key (description)=(Guarding) already exists.`

## 3.

### A)

```sql
INSERT INTO Robbers (RobberId, Nickname, Age, NoYears)
    VALUES (1, 'Shotgun', 70, 0);
```

Error produced:

`ERROR:  duplicate key value violates unique constraint "robbers_pkey"
DETAIL:  Key (robberid)=(1) already exists.`

### B)

```sql
INSERT INTO Robbers (RobberId, Nickname, Age, NoYears)
    VALUES (333, 'Jail Mouse', 25, 35);
```

Error produced:

`ERROR:  new row for relation "robbers" violates check constraint "age_gt_noyears"
DETAIL:  Failing row contains (333, Jail Mouse, 25, 35).`

## 4.

### A)

```sql
INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (333, 1, 1, 'B-');
```

Error produced:

`ERROR:  insert or update on table "hasskills" violates foreign key constraint "hasskills_robberid_fkey"
DETAIL:  Key (robberid)=(333) is not present in table "robbers".`

### B)


```sql
INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (3, 20, 3, 'B+');
```

Error produced:

`ERROR:  insert or update on table "hasskills" violates foreign key constraint "hasskills_skillid_fkey"
DETAIL:  Key (skillid)=(20) is not present in table "skills".`

### C)


```sql
INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (1, 7, 1, 'A+');
```

Error produced:

`ERROR:  duplicate key value violates unique constraint "unique_robber_pref"
DETAIL:  Key (robberid, preference)=(1, 1) already exists.`

### D)


```sql
INSERT INTO HasSkills (RobberId, SkillId, Preference, Grade)
    VALUES (1, 2, 0, 'A');
```

Error produced:

`ERROR:  new row for relation "hasskills" violates check constraint "pref_positive"
DETAIL:  Failing row contains (1, 2, 0, A).`

## 5.

## A)

```sql
INSERT INTO Robberies
    VALUES ('NXP Bank', 'Chicago', '2009-01-08', 1000);
```

Error produced:

`ERROR:  duplicate key value violates unique constraint "uniq_robberies"
DETAIL:  Key (bankname, city, "Date")=(NXP Bank, Chicago, 2009-01-08) already exists.`

## 6.

### A)

```sql
DELETE FROM Banks WHERE
    BankName = 'PickPocket Bank' AND
    City = 'Evanston' AND
    NoAccounts = 2000 AND
    Security = 'very good';
```

There was no error.
The tuple targeted was deleted from the table, and since
the tables referencing this table are set to `ON DELETE CASCADE`
they were also deleted.
If the references were setup differently, e.g. using `RESTRICT`
then an error of foreign key constraints will be raised.

### B)

```sql
DELETE FROM Banks WHERE
    BankName = 'Outside Bank' AND
    City = 'Chicago' AND
    NoAccounts = 5000 AND
    Security = 'good';
```


There was no error.
The tuple targeted was deleted from the table, and since
the tables referencing this table are set to `ON DELETE CASCADE`
they were also deleted.
If the references were setup differently, e.g. using `RESTRICT`
then an error of foreign key constraints will be raised.

## 7.

### A)

```sql
DELETE FROM Robbers WHERE
    RobberId = 1 AND
    Nickname = 'Al Capone' AND
    Age = 31 AND
    NoYears = 2;
```

There was no error.
The tuple targeted was deleted from the table, and since
the tables referencing this table are set to `ON DELETE CASCADE`
they were also deleted.
If the references were setup differently, e.g. using `RESTRICT`
then an error of foreign key constraints will be raised.

## 8.

### A)

```sql
DELETE FROM Skills WHERE
    SkillId = 1 AND
    Description = 'Driving';
```

There was no error.
No tuples were deleted by this query as there is no complete
match for it in the database.

# Question 4

## 1)

```sql
SELECT BankName, Security FROM Banks WHERE NoAccounts > 9000;
```

### Results

BankName         | Security
-----------------+-----------
 NXP Bank        | very good
 Bankrupt Bank   | weak
 Loanshark Bank  | excellent
 Loanshark Bank  | very good
 Loanshark Bank  | excellent
 Inter-Gang Bank | excellent
 Inter-Gang Bank | excellent
 NXP Bank        | excellent
 Penny Pinchers  | weak
 Dollar Grabbers | very good
 Penny Pinchers  | excellent
 Dollar Grabbers | good
 Gun Chase Bank  | excellent
 PickPocket Bank | weak
 Hidden Treasure | excellent

## 2)

```sql
SELECT BankName FROM HasAccounts NATURAL JOIN Robbers WHERE Nickname = 'Calamity Jane';
```

### Results

| BankName      |
|---------------|
|PickPocket Bank|
|Bad Bank       |
|Dollar Grabbers|

## 3)

```sql
SELECT BankName, City FROM Banks
WHERE BankName NOT IN
    (SELECT DISTINCT BankName FROM Banks WHERE City = 'Chicago')
ORDER BY NoAccounts;
```

### Results

 BankName       |   City
----------------+-----------
 Gun Chase Bank | Deerfield
 Bankrupt Bank  | Evanston
 Gun Chase Bank | Evanston

## 4)

```sql
SELECT BankName, City FROM Banks
NATURAL JOIN Robberies
ORDER BY "Date" LIMIT 1;
```

### Results

 BankName       |   City
----------------+----------
 Loanshark Bank | Evanston

## 5)

```SQL
SELECT * FROM
    (SELECT NickName, SUM("Share") AS Earnings
     FROM Robbers NATURAL JOIN Accomplices
     GROUP BY RobberId) AS RobberEarnings
WHERE Earnings > '30000'::money
ORDER BY Earnings DESC;
```

### Results

 Nickname          |  Earnings
-------------------+------------
 Mimmy The Mau Mau | \$70,000.00
 Boo Boo Hoff      | \$61,447.61
 King Solomon      | \$59,725.80
 Bugsy Siegel      | \$52,601.10
 Lucky Luchiano    | \$42,667.00
 Bonnie            | \$40,085.00
 Anastazia         | \$39,169.62
 Clyde             | \$31,800.00

## 6)

```sql
SELECT Description, RobberId, Nickname
FROM HasSkills NATURAL JOIN Robbers NATURAL JOIN Skills
ORDER BY description;
```

### Results

 Description    | RobberId | Nickname
----------------+----------+-------------------
 Cooking        |       18 | Vito Genovese
 Driving        |        3 | Lucky Luchiano
 Driving        |       23 | Lepke Buchalter
 Driving        |       20 | Longy Zwillman
 Driving        |        5 | Mimmy The Mau Mau
 Driving        |       17 | Bugsy Siegel
 Driving        |        7 | Dutch Schulz
 Eating         |       18 | Vito Genovese
 Eating         |        6 | Tony Genovese
 Explosives     |        2 | Bugsy Malone
 Explosives     |       24 | Sonny Genovese
 Guarding       |       17 | Bugsy Siegel
 Guarding       |        4 | Anastazia
 Guarding       |       23 | Lepke Buchalter
 Gun-Shooting   |        9 | Calamity Jane
 Gun-Shooting   |       21 | Waxey Gordon
 Lock-Picking   |       22 | Greasy Guzik
 Lock-Picking   |        3 | Lucky Luchiano
 Lock-Picking   |        7 | Dutch Schulz
 Lock-Picking   |        8 | Clyde
 Lock-Picking   |       24 | Sonny Genovese
 Money Counting |       14 | Kid Cann
 Money Counting |       13 | Mickey Cohen
 Money Counting |       19 | Mike Genovese
 Planning       |        5 | Mimmy The Mau Mau
 Planning       |       15 | Boo Boo Hoff
 Planning       |       16 | King Solomon
 Planning       |        8 | Clyde
 Preaching      |       22 | Greasy Guzik
 Preaching      |       10 | Bonnie
 Safe-Cracking  |       11 | Meyer Lansky
 Safe-Cracking  |       12 | Moe Dalitz
 Safe-Cracking  |       24 | Sonny Genovese
 Scouting       |        8 | Clyde
 Scouting       |       18 | Vito Genovese


## 7)

```sql
SELECT RobberId, Nickname FROM Robbers WHERE NoYears > 3;
```

### Results

 RobberId |    nickname
----------+----------------
        2 | Bugsy Malone
        3 | Lucky Luchiano
        4 | Anastazia
        6 | Tony Genovese
        7 | Dutch Schulz
       11 | Meyer Lansky
       15 | Boo Boo Hoff
       16 | King Solomon
       17 | Bugsy Siegel
       20 | Longy Zwillman


## 8)

```sql
SELECT RobberId, Nickname, (Age - NoYears) AS NotInPrison
FROM Robbers WHERE (NoYears * 2 >= Age);
```


### Results

 RobberId |   Nickname    | NotInPrison
----------+---------------+-------------
        6 | Tony Genovese |          12
       16 | King Solomon  |          31

# Question 5

## 1

### Step-wise

```sql
CREATE VIEW ParticipatedIn AS 
    SELECT RobberId, COUNT(RobberId) as participated, SUM("Share") as earnings FROM accomplices 
    GROUP BY robberid;

CREATE VIEW AvgParticipation AS
    SELECT AVG(Participated) as AvgPart FROM ParticipatedIn;


CREATE VIEW AboveAvgParticipation AS
    SELECT Nickname FROM 
    Robbers NATURAL JOIN ParticipatedIn NATURAL JOIN AvgParticipation
    WHERE (AvgPart < Participated) AND (NoYears = 0)
    ORDER BY Earnings DESC;

SELECT * FROM AboveAvgParticipation;
```

#### Results

|Nickname      |
|--------------|
|Bonnie        |
|Clyde         |
|Sonny Genovese|


### Nest queries

```sql
SELECT Nickname FROM
Robbers NATURAL JOIN
    (SELECT RobberId, COUNT(RobberId) as Part, SUM("Share") as Earn FROM Accomplices
    GROUP BY RobberID) AS P
    NATURAL JOIN
    (SELECT AVG(Pa) AS PartAvg FROM
        (SELECT RobberId, COUNT(RobberId) as Pa FROM Accomplices
        GROUP BY RobberID) AS PP
    ) AS AV
WHERE (PartAvg < Part) AND (NoYears = 0)
ORDER BY Earn DESC;
```

#### Results

|Nickname      |
|--------------|
|Bonnie        |
|Clyde         |
|Sonny Genovese|

## 2

### Step-wise

```sql
CREATE VIEW BankRobberies AS
    SELECT Security, Count(*) as TotalRobberies, AVG(Amount::numeric) as AvgAmount
    FROM robberies NATURAL JOIN Banks GROUP BY Security;

SELECT * FROM BankRobberies;
```

#### Results

 Security  | TotalRobberies | AvgAmount
-----------+----------------+-----------
 good      |              2 | 3980.00
 weak      |              4 | 2299.50
 excellent |             12 | 39238.083
 very good |              1 | 34302.30

### Nest queries

```sql
SELECT Security, Count(*) as TotalRobberies, AVG(Amount::numeric) as AvgAmount
    FROM robberies NATURAL JOIN Banks GROUP BY Security;
```

#### Results

 Security  | TotalRobberies | AvgAmount
-----------+----------------+-----------
 good      |              2 | 3980.00
 weak      |              4 | 2299.50
 excellent |             12 | 39238.083
 very good |              1 | 34302.30

## 3

### Step-wise

```sql
CREATE VIEW RobberSkills AS
    SELECT RobberId, Nickname, Description FROM
    Robbers NATURAL JOIN HasSkills NATURAL JOIN Skills;

CREATE VIEW RobberySecurity AS
    SELECT RobberId, Security FROM
    Robbers NATURAL JOIN Accomplices NATURAL JOIN Banks;

CREATE VIEW SkillsUsed AS
    SELECT Security, Description, Nickname FROM
    RobberSkills NATURAL JOIN RobberySecurity;

SELECT DISTINCT * FROM SkillsUsed;
```

#### Results

 Security  |  Description   | Nickname      
-----------+----------------+-------------------
 weak      | Cooking        | Vito Genovese
 weak      | Driving        | Bugsy Siegel
 weak      | Driving        | Dutch Schulz
 weak      | Driving        | Lepke Buchalter
 weak      | Eating         | Vito Genovese
 weak      | Explosives     | Sonny Genovese
 weak      | Guarding       | Bugsy Siegel
 weak      | Guarding       | Lepke Buchalter
 weak      | Lock-Picking   | Clyde
 weak      | Lock-Picking   | Dutch Schulz
 weak      | Lock-Picking   | Greasy Guzik
 weak      | Lock-Picking   | Sonny Genovese
 weak      | Planning       | Boo Boo Hoff
 weak      | Planning       | Clyde
 weak      | Preaching      | Greasy Guzik
 weak      | Safe-Cracking  | Sonny Genovese
 weak      | Scouting       | Clyde
 weak      | Scouting       | Vito Genovese
 good      | Cooking        | Vito Genovese
 good      | Eating         | Vito Genovese
 good      | Money Counting | Kid Cann
 good      | Money Counting | Mickey Cohen
 good      | Scouting       | Vito Genovese
 very good | Driving        | Lepke Buchalter
 very good | Driving        | Longy Zwillman
 very good | Explosives     | Bugsy Malone
 very good | Guarding       | Anastazia
 very good | Guarding       | Lepke Buchalter
 very good | Planning       | King Solomon
 excellent | Driving        | Bugsy Siegel
 excellent | Driving        | Dutch Schulz
 excellent | Driving        | Longy Zwillman
 excellent | Driving        | Lucky Luchiano
 excellent | Driving        | Mimmy The Mau Mau
 excellent | Explosives     | Sonny Genovese
 excellent | Guarding       | Anastazia
 excellent | Guarding       | Bugsy Siegel
 excellent | Gun-Shooting   | Waxey Gordon
 excellent | Lock-Picking   | Clyde
 excellent | Lock-Picking   | Dutch Schulz
 excellent | Lock-Picking   | Greasy Guzik
 excellent | Lock-Picking   | Lucky Luchiano
 excellent | Lock-Picking   | Sonny Genovese
 excellent | Planning       | Boo Boo Hoff
 excellent | Planning       | Clyde
 excellent | Planning       | King Solomon
 excellent | Planning       | Mimmy The Mau Mau
 excellent | Preaching      | Bonnie
 excellent | Preaching      | Greasy Guzik
 excellent | Safe-Cracking  | Meyer Lansky
 excellent | Safe-Cracking  | Sonny Genovese
 excellent | Scouting       | Clyde


### Nest queries

```sql
SELECT DISTINCT Security, Description, Nickname FROM
    Robbers NATURAL JOIN HasSkills NATURAL JOIN Skills
    NATURAL JOIN Accomplices NATURAL JOIN Banks;
```

#### Results

 Security  |  Description   | Nickname      
-----------+----------------+-------------------
 weak      | Cooking        | Vito Genovese
 weak      | Driving        | Bugsy Siegel
 weak      | Driving        | Dutch Schulz
 weak      | Driving        | Lepke Buchalter
 weak      | Eating         | Vito Genovese
 weak      | Explosives     | Sonny Genovese
 weak      | Guarding       | Bugsy Siegel
 weak      | Guarding       | Lepke Buchalter
 weak      | Lock-Picking   | Clyde
 weak      | Lock-Picking   | Dutch Schulz
 weak      | Lock-Picking   | Greasy Guzik
 weak      | Lock-Picking   | Sonny Genovese
 weak      | Planning       | Boo Boo Hoff
 weak      | Planning       | Clyde
 weak      | Preaching      | Greasy Guzik
 weak      | Safe-Cracking  | Sonny Genovese
 weak      | Scouting       | Clyde
 weak      | Scouting       | Vito Genovese
 good      | Cooking        | Vito Genovese
 good      | Eating         | Vito Genovese
 good      | Money Counting | Kid Cann
 good      | Money Counting | Mickey Cohen
 good      | Scouting       | Vito Genovese
 very good | Driving        | Lepke Buchalter
 very good | Driving        | Longy Zwillman
 very good | Explosives     | Bugsy Malone
 very good | Guarding       | Anastazia
 very good | Guarding       | Lepke Buchalter
 very good | Planning       | King Solomon
 excellent | Driving        | Bugsy Siegel
 excellent | Driving        | Dutch Schulz
 excellent | Driving        | Longy Zwillman
 excellent | Driving        | Lucky Luchiano
 excellent | Driving        | Mimmy The Mau Mau
 excellent | Explosives     | Sonny Genovese
 excellent | Guarding       | Anastazia
 excellent | Guarding       | Bugsy Siegel
 excellent | Gun-Shooting   | Waxey Gordon
 excellent | Lock-Picking   | Clyde
 excellent | Lock-Picking   | Dutch Schulz
 excellent | Lock-Picking   | Greasy Guzik
 excellent | Lock-Picking   | Lucky Luchiano
 excellent | Lock-Picking   | Sonny Genovese
 excellent | Planning       | Boo Boo Hoff
 excellent | Planning       | Clyde
 excellent | Planning       | King Solomon
 excellent | Planning       | Mimmy The Mau Mau
 excellent | Preaching      | Bonnie
 excellent | Preaching      | Greasy Guzik
 excellent | Safe-Cracking  | Meyer Lansky
 excellent | Safe-Cracking  | Sonny Genovese
 excellent | Scouting       | Clyde


## 4

### Step-wise

```sql
CREATE VIEW PlannedRobberies AS
    SELECT BankName, City, Security, PlannedDate FROM
    Banks NATURAL JOIN Plans
    WHERE PlannedDate < (NOW() + interval '1 year')::Date AND
          PlannedDate > NOW();

CREATE VIEW RecentRobberies AS
    SELECT BankName, City, Security, "Date" FROM
    Banks NATURAL JOIN Robberies
    WHERE "Date" > (NOW() - interval '1 year')::Date AND
          "Date" < NOW()::Date;

CREATE VIEW SoonVictims AS
    SELECT P.BankName, P.City, P.Security FROM
    PlannedRobberies as P LEFT JOIN RecentRobberies as R
    ON R.Bankname = P.Bankname AND R.City = P.City;

SELECT * FROM SoonVictims;
```

#### Results

 BankName        |   City    | Security  
-----------------+-----------+-----------
 Loanshark Bank  | Deerfield | very good
 PickPocket Bank | Deerfield | excellent
 Bad Bank        | Chicago   | weak

### Nest queries

```sql
SELECT P.BankName, P.City, P.Security FROM
    (SELECT BankName, City, Security, PlannedDate FROM
    Banks NATURAL JOIN Plans
    WHERE PlannedDate < (NOW() + interval '1 year')::Date AND
    PlannedDate > NOW()) AS P
LEFT JOIN
    (SELECT BankName, City, Security, "Date" FROM
    Banks NATURAL JOIN Robberies
    WHERE "Date" > (NOW() - interval '1 year')::Date AND
          "Date" < NOW()::Date) AS R
    ON R.Bankname = P.Bankname AND R.City = P.City;
```

#### Results

 BankName        |   City    | Security  
-----------------+-----------+-----------
 Loanshark Bank  | Deerfield | very good
 PickPocket Bank | Deerfield | excellent
 Bad Bank        | Chicago   | weak


## 5

### Step-wise

```sql
CREATE VIEW AvgShareByCity AS
    SELECT City, AVG("Share"::numeric) AS AvgShare FROM
        Accomplices
        GROUP BY City;

CREATE VIEW DistrictSummary AS
    SELECT 'Other'::text AS City, AVG(AvgShare) AS DistrictShare FROM
    AvgShareByCity
    GROUP BY City
    HAVING (City != 'Chicago');

CREATE VIEW chicagosummary AS
    SELECT City, AVG(AvgShare) AS DistrictShare FROM
    AvgShareByCity
    GROUP BY City
    HAVING (City = 'Chicago');
    
CREATE VIEW Summary AS
    SELECT * FROM DistrictSummary
    UNION
    SELECT * FROM ChicagoSummary;

SELECT * FROM Summary;
```

#### Results

  City   |     DistrictShare     
---------+-----------------------
 Chicago | 4221.41
 Other   | 8255.15


### Nest queries

```sql
```

