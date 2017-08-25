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

![Expression tree of query](./q2_2_tree.png)

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

![Expression tree of query](./q3_a_tree.png)

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

# Question 3

## A)

## B)
