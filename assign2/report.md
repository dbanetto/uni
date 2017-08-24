% SWEN304 - Assignment 2
% David Barnett (300313764)

# Question 1

## A)

### 1)

#### Relational Algebra 

> $\pi_{Mus\_Name} ( \sigma_{Mus\_Type = "Conductor"} ( r(Musician) ) )$

#### Tutorial D

### 2)

#### Relational Algebra 

> $\pi_{Mus\_Name, Inst\_Name} ( \sigma_{Mus\_Type = "Singer"} ( r(Musician) ) \ast r(Playd\_By) \ast r(Instrument) )$

#### Tutorial D

### 3)

#### Relational Algebra 

> $(\pi_{Mus\_Name} ( \sigma_{Inst\_Name = "Piano"} ( r(Musician) \ast ( r(Instrument) \ast r(Played\_By) ) ) ) \cup \pi_{Mus\_Name} ( \sigma_{Inst\_Name = "Violin"} ( r(Musician) \ast ( r(Instrument) \ast r(Played\_By) ) ) )) - \pi_{Mus\_Name} ( \sigma_{Inst\_Name = "Guitar"} ( r(Musician) \ast ( r(Instrument) \ast r(Played\_By) ) ) )$

#### Tutorial D

## B)

### 1)

#### English

Retrieve the musician id and name of all musicians that do not
play an instrument.

#### Tutorial D

### 2)

#### English

Retrieve the Instrument id and instrument name from
the aggregate count of the natural join between instruments 
and whom they are played by.

#### Tutorial D

# Question 2

## A)

### 1)

$\pi_{StudentId, Name, NoOfPts, CourName} (
 \sigma_{NoOfPts > 200 \wedge CourseId = 'SWEN304'}
(
    r(Student) \ast r(Enrolled) \ast r(Course)
))$


### 2)



### 3)

\begin{align*}
    &\pi_{(StudentId, Name, NoOfPts, CourName)} ( \\
    &\pi_{(StudentId, Name, NoOfPts)} (\sigma_{(NoOfPts > 200)}(r(Student))) \ast \\
    &(\pi_{(StudentId, CourseId)} (r(Enrolled)) \ast \\
    &\pi_{(CourseId, CourName)} (\sigma_{(CourseId = 'SWEN304')} ( r(Course) ) )))
\end{align*}

## B)

### 1)

### 2)

# Question 3

## A)

## B)
