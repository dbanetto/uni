% SWEN304 - Assignment 2
% David Barnett (300313764)

# Question 1

## A)

### 1)

#### Relational Algebra

\begin{equation*}
\pi_{Mus\_Name} ( \sigma_{Mus\_Type = "Conductor"} ( r(Musician) ) )
\end{equation*}


#### Tutorial D

```
(Musician WHERE Mus_Type = 'Conductor') { Mus_Name }
```

### 2)

#### Relational Algebra

\begin{equation*}
\pi_{Mus\_Name, Inst\_Name} ( \sigma_{Mus\_Type = "Singer"} ( r(Musician) ) \ast r(Played\_By) \ast r(Instrument) )
\end{equation*}

#### Tutorial D

```
((Musician JOIN (Played_By JOIN Instrument)) WHERE (Mus_Type = "Singer")) {Mus_Name, Inst_Name}
```

### 3)

#### Relational Algebra

\begin{equation*}
    \begin{split}
       \\
        (&\pi_{Mus\_Name} ( \sigma_{Inst\_Name = "Piano"} ( r(Musician) \ast ( r(Instrument) \ast r(Played\_By) ) ) ) \\
        \cup \\
        &\pi_{Mus\_Name} ( \sigma_{Inst\_Name = "Violin"} ( r(Musician) \ast ( r(Instrument) \ast r(Played\_By) ) ))) \\
         - \\
         &\pi_{Mus\_Name} ( \sigma_{Inst\_Name = "Guitar"} ( r(Musician) \ast ( r(Instrument) \ast r(Played\_By) ) ) )
\end{split}
\end{equation*}

#### Tutorial D

```
((((Musician JOIN (Played_By JOIN Instrument)) WHERE (Inst_Name = 'Piano')) UNION
((Musician JOIN (Played_By JOIN Instrument)) WHERE (Inst_Name = 'Violin')))
MINUS
((Musician JOIN (Played_By JOIN Instrument)) WHERE (Inst_Name = 'Guitar'))) {Mus_Name}
```

## B)

### 1)

#### English

Retrieve the musician id and name of all musicians that do not
play an instrument.

#### Tutorial D

```
((Musician) {MusicianId, Mus_Name}) MINUS ((Musician JOIN Played_By) {MusicianId, Mus_Name})
```

### 2)

#### English

Retrieve the Instrument id and instrument name from
the aggregate count of the natural join between instruments
and whom they are played by.

#### Tutorial D

```
SUMMARIZE (Instrument JOIN Played_By) PER ((Instrument JOIN Played_By) {InstrumentId, Inst_Name} )
ADD (COUNT() AS C)
```

# Question 2

## A)

### 1)

$\pi_{StudentId, Name, NoOfPts, CourName} (
 \sigma_{NoOfPts > 200 \wedge CourseId = 'SWEN304'}
(
    r(Student) \ast r(Enrolled) \ast r(Course)
))$


### 2)

See figure 1.

![Expression tree of query for Question 2.a](./q2_1_tree.png)

### 3)

\begin{align*}
    &\pi_{(StudentId, Name, NoOfPts, CourName)} ( \\
    &\pi_{(StudentId, Name, NoOfPts)} (\sigma_{(NoOfPts > 200)}(r(Student))) \ast \\
    &\pi_{StudentId, CourName} (\pi_{(StudentId, CourseId)} (r(Enrolled)) \ast \\
    &\pi_{(CourseId, CourName)} (\sigma_{(CourseId = 'SWEN304')} ( r(Course) ) )))
\end{align*}

## B)

### 1)

#### Values

$r_s = 20000$
$r_e = 300000$

The size of a tuple in the Students table is

Column      | Size (bytes)
------------+--------------
`StudentId` | 4\*
`Name`      | 15\dag{}
`NoOfPts`   | 2
`Tutor`     | 4\*

Total: $1 + 15 + 2 + 4 = 25\text{bytes}$

The size of a tuple in the Enrolled table is

Column      | Size (bytes)
------------+--------------
`StudentId` | 4\*
`CourseId`  | 4\dag{}
`Term`      | 2
`Grade`     | 2

Total: $4 + 4 + 2 + 2 = 12\text{bytes}$

\pagebreak

\begin{equation*}
\begin{split}
    n &= \lfloor \frac{buffer size}{block size} \rfloor \\
      &= \lfloor \frac{4000}{500} \rfloor \\
      &= 8
\end{split}
\end{equation*}

\begin{equation*}
\begin{split}
    f &= \lfloor \frac{buffer size}{tuple size} \rfloor \\
      &= \lfloor \frac{500}{25 + 12} \rfloor \\
      &= 13
\end{split}
\end{equation*}


**Assumptions**

> \*: Assuming the `PRIMARY KEY` type is 4 bytes

> \dag: Assuming a `char` type is 1 byte (i.e is ASCII instead of UTF-8 or Unicode encoded)


#### Relational algebra of selection:

$r(Student) \Join_{Student.StudentId = Enrolled.StudentId} r(Enrolled)$

#### Expression tree of query

See figure 2

![Expression tree of query for Question 2.b.1](./q2_2_tree.png)

#### Cost function

\begin{equation*}
    C = b_n + b_m \lceil \frac{b_n}{n - 2} \rceil + \lceil \frac{r_m}{f} \rceil
\end{equation*}

Solving for $Student \Join Enrolled$

\begin{equation*}
\begin{split}
    C &= b_s + b_e \lceil \frac{b_s}{n - 2} \rceil + \lceil \frac{r_e}{f} \rceil \\
      &= 1000 + 7200 \lceil \frac{1000}{8 - 2} \rceil + \lceil \frac{300000}{13} \rceil \\
      &= 1000 + 1202400 + 23077 \\
      &= 1226477
\end{split}
\end{equation*}

Solving for $Enrolled \Join Student$

\begin{equation*}
\begin{split}
    C &= b_e + b_s \lceil \frac{b_e}{n - 2} \rceil + \lceil \frac{r_s}{f} \rceil \\
      &= 7200 + 1000 \lceil \frac{7200}{8 - 2} \rceil + \lceil \frac{20000}{13} \rceil \\
      &= 7200 + 1200000 + 1539 \\
      &= 1208739
\end{split}
\end{equation*}

The query of $Enrolled \Join Student$ has the lowest cost with the nested-loop
join

### 2)

#### Relational Algebra expression

\begin{equation*}
    \sigma_{Term = 2014 \wedge CourseId = 'SWEN304' } (r(Enrolled))
\end{equation*}

#### Expression tree

See figure 3

![Expression tree of query for Question 2.b.2](./q2_3_tree.png)

#### Calculating Cost

$b$ is the number of blocks of the input relation

In this case $b = 7200$

$s$ is the selection cardinality of the search argument Y

It is assumed that each year equal number of enrollments and
that in each year there are even enrollments into each course.
This implies the expected cardinality of the selection is
$(300000 / (2017 - 1997)) / 500 = 30$. In this case $s = 30$

$f$ is the block factor

\begin{equation*}
\begin{split}
    f &= \lfloor \frac{buffer size}{tuple size} \rfloor \\
      &= \lfloor \frac{500}{12} \rfloor \\
      &= 41
\end{split}
\end{equation*}

##### Linear Search

\begin{equation*}
\begin{split}
    C = b + \lceil \frac{s(Y)}{f} \rceil
\end{split}
\end{equation*}

\begin{equation*}
\begin{split}
    C &= b + \lceil \frac{s(Y)}{f} \rceil \\
      &= 7200 + \lceil \frac{30}{41} \rceil \\
      &= 7200 + 1 \\
      &= 7201
\end{split}
\end{equation*}

##### Index Search

Given that $m = 8$ and $h = 7$

\begin{equation*}
\begin{split}
    C &= h + \lceil  \frac{s}{m} \rceil + s + \lceil  \frac{s}{f} \rceil \\
      &= 7 + \lceil  \frac{30}{8} \rceil + 30 + \lceil  \frac{30}{41} \rceil \\
      &= 7 + 4 + 30 + 1 \\
      &= 42
\end{split}
\end{equation*}

#### Discussion

In this case the Index search is faster as the two attributes being queried on
are part of the primary key of the relation.
This resulted in a ~1000 times speed up between the two algorithms.

# Question 3

**Note: These queries were performed on
my own postgres instance running 9.6.4**

## A)

The results of the query is shown below

```
swen304_a2=# explain select count(*) from customer where no_borrowed = 6;
                           QUERY PLAN
-----------------------------------------------------------------
 Aggregate  (cost=114.41..114.42 rows=1 width=8)
   ->  Seq Scan on customer  (cost=0.00..114.25 rows=63 width=0)
         Filter: (no_borrowed = 6)
(3 rows)
```

The type of `no_borrowed` was changed from `integer` to `smallint` as the number
of borrowed items would not reasonable exceed 32767 (upper limit of a `smallint`).
After adding an index to the table via the SQL below yielded the `explain` to give the
estimated cost of 4.83.

```sql
CREATE INDEX ON customer (no_borrowed);
```

```
swen304_a2=# explain select count(*) from customer where no_borrowed = 6;
                                             QUERY PLAN
-----------------------------------------------------------------------------------------------------
 Aggregate  (cost=4.82..4.83 rows=1 width=8)
   ->  Index Only Scan using customer_no_borrowed_idx on customer  (cost=0.28..4.75 rows=27 width=0)
         Index Cond: (no_borrowed = 6)
(3 rows)
```

This is a 95.78% speed up compared to the original query.
This speed up has occurred by the DBMS indexing each tuple in the relation
into a table index via the `no_borrowed` attribute, with this the values
are grouped into buckets of the same values which cause the estimated cost to
fall by such large margins.

## B)

Below are the original results of the query plan.

```
swen304_a2=# explain select * from customer where customerid = 4567;
                        QUERY PLAN
----------------------------------------------------------
 Seq Scan on customer  (cost=0.00..52.94 rows=1 width=49)
   Filter: (customerid = 4567)
(2 rows)
```

An attempt to reduce to the size of a tuple was to move the
repeating city names into a separate table such that `cusomter` had
replaced the `city` column for `cityid` that is a foreign key to a `city`
table that contains `name` and `cityid`.
By making the customer id a primary key there was a speedup
of 84.32% for the query. The results of the `EXPLAIN` are below.


```
swen304_a2=# explain select * from customer where customerid = 4567;
                                  QUERY PLAN
-------------------------------------------------------------------------------
 Index Scan using customer_pkey on customer  (cost=0.28..8.30 rows=1 width=44)
   Index Cond: (customerid = 4567)
(2 rows)
```

This speed up has occurred by the making the `customerid` a primary key since the
DBMS can make the assumption that that value is unique and can speed up the search
for it using B+ trees or other indexing data structures.

## C)
