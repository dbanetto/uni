% NWEN303 - Assignment 1
% David Barnett (300313764)

# 1. Synchronisation 
<!--
K and finished are global / shared variables
-->

## A)
<!--
race condition
where the output is dependent on the sequence or timing of other uncontrollable events
- race:
    k is overwritten by each thread
    a loop may start while found is being assigned
    multiple threads could find a result at the same time

Types:
- check-then-act
- read-modify-write
-->

A race condition is when the result of an operation is dependent on 
sequence of execution of two or more threads. Race conditions can come in many types
and may break the correctness of the operation. The given algorithm to concurrently 
find were `f(x)` equals 0 has a few race conditions.

One race condition results from the algorithm shares the variable `k` between all threads
which is set to be the starting point for the thread to find the answer. The race condition
comes from the series of values actually checked is dependent on which is the last thread
to start and set `k` to be its starting point and every other thread is then set to be this
value as well. This will cause the algorithm to only check `1/N` of the search space it was
suppose to. 

There is another race condition that results from having `k` shared between all the threads.
This could occur when one thread finds a value of `k` and in the time between starting the
execution of the check of `f(k) == 0` and printing the `k` another thread could modify
`k` due to that thread failed its check for `f(k) == 0` because another thread updating
`k` in between these checks. This race condition is a combination of check-then-act and 
read-modify-write races. This would cause incorrect results from the algorithm.

A result of having the `found` boolean being shared a race condition is introduced.
This race condition is cause when a thread has  just found a correct value of `k` and
is about to update the `found` variable but another thread could start another loop
in between the time of find the result. This would cause an extra loop to be executed,
which in this context is not expensive and is a reasonable cost. However if in that extra
loop the thread finds another solution it will result in more than 1 result for the algorithm
which is not desired. 

## B)
<!--
modify to avoid reace condition,
correct without race nor deadlocks 

-   make k to be a thread local variable
-->

There are a few modifications I would make to the given algorithm to avoid the race
conditions currently present. 

The most major modification would be to make `k` a thread local variable.
This will prevent each thread from over writing each others starting values or current values
between checking `f(k) == 0` and printing `k`. 
This would greatly increase the correctness of the algorithm. 

Depending the on the acceptability of the algorithm finding more than 1 solution the `found` 
might need to be locked. Given that only 1 solution is acceptable the `found` variable will
need to synchronise in the success branch of `f(k) == 0` and preform an extra check for if
`found` is true before printing the result to prevent a check-then-act race condition.
This will result in a chance of an extra loop to be executed if the loop starts just before the
`found` variable is updated but to prevent this it would require locking found as it is being
read which would hurt the performance of the algorithm. 

## C)
<!--
fairness, how does B) depend on fairness assumption
- same amount of process time
- fair share of processing time between threads
- pusdeo stravation
-->

Fairness in concurrency is when each threads is an even share
of the available resources such as CPU time, memory or CPU cache.
The correctness of my algorithm in part B) relies on each thread being
given CPU time to progress. In an unfair situation on thread could
be given CPU time sparingly and end up being behind the other threads
as they progress further in their allocated work. This could cause the
correctness of the algorithm to suffer if one thread that would eventually
find the solution is not run.

## D)
<!--
how could algorithm from B) could be modified to work
with absence of fairness. How does it impact performace
-->

In the absence of fairness the algorithm from B) could be modified to remove this
assumption. One possible way of doing this is by having a shared queue of available
values to be checked. Instead of being given a starting value to increment each thread
would just dequeue a value to check from the shared work queue. This would result in 
some loss of performance as threads might have to wait on a lock on the queue when getting
values from it, depending on the implementation of the queue.

# 2. Measuring Array Sum

# 3. Recursive Array Sum
<!--
Extra: thread pool, executor service
-->
