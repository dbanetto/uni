% David Barnett (300313264)
% NWEN303 - Assignment 2

# 1. Readers and Writers

Base solution from the lecture slides:

The general program used with reader/writer

Reader                Writer
--------------------  ----------------------
loop                  loop
    r1: NCS               w1: NCS
    r2: RW.StartR         w2: RW.StartW
    r3: read              w3: read & write
    r4: RW.EndR           w4: RW.EndW

The basic monitor is:

```
monitor RW
    int nr := 0
    int nw := 0
    cond OKR, OKW

    op StartR
        if nw != 0 or !empty(OKW)
            waitC(OKR)
        nr := nr + 1
        signalC(OKR)

    op EndR
        nr := nr - 1
        if nr = 0
            signalC(OKW)


    op StartW
        if nw!=0 or nr!=0
            waitC(OKW)
        nw := nw + 1

    op EndW
        nw := nw - 1
        if empty(OKR)
            signalC(OKW)
        else
            signalC(OKR)
```

\pagebreak

## A - Readers take precedences


```
monitor RW
    int nr := 0
    int nw := 0
    cond OKR, OKW

    op StartR
        if nw != 0
            waitC(OKR)
        nr := nr + 1
        signalC(OKR)

    op EndR
        nr := nr - 1
        if nr = 0
            signalC(OKW)


    op StartW
        if nw!=0 or nr!=0
            waitC(OKW)
        nw := nw + 1

    op EndW
        nw := nw - 1
        if empty(OKR)
            signalC(OKW)
        else
            signalC(OKR)
```

To make the readers enter the critical section even though there is a
writing thread waiting the check for `!empty(OKW)` in the `StartR` check for
writers is needed to be removed to get this functionality.
This is at the cost of potent ional starvation for writer threads as readers
could continually jump ahead of them and never be able to take a step.

\pagebreak

## B - Waiting writers over waiting readers

```
monitor RW
    int nr := 0
    int nw := 0
    cond OKR, OKW

    op StartR
        if nw != 0 or !empty(OKW)
            waitC(OKR)
        nr := nr + 1
        signalC(OKR)

    op EndR
        nr := nr - 1
        if nr = 0
            signalC(OKW)


    op StartW
        if nw!=0 or nr!=0
            waitC(OKW)
        nw := nw + 1

    op EndW
        nw := nw - 1
        if empty(OKW)
            signalC(OKR)
        else
            signalC(OKR)
```

The only change needed to make sure any waiting writer will be executed before a
reader is executed before any waiting readers is modifying the `EndW` operation
to only signal `OKR` when there is no waiting writes.
This will come at the cost of introducing starvation as continually adding new
writing threads to the queue will prevent any reading threads from taking a step.

\pagebreak

# C - Only allow 2 waiting writes to proceed before read

```
monitor RW
    int nr := 0
    int nw := 0
    int ww := 0
    cond OKR, OKW

    op StartR
        if nw != 0 or !empty(OKW)
            waitC(OKR)
        nr := nr + 1
        signalC(OKR)

    op EndR
        nr := nr - 1
        if nr = 0
            signalC(OKW)


    op StartW
        if nw!=0 or nr!=0
            waitC(OKW)
        nw := nw + 1

    op EndW
        nw := nw - 1
        if !empty(OKW) && ww < 2
            ww := ww + 1
            signalC(OKW)
        else if empty(OKR)
            ww := 0
            signalC(OKW)
        else
            ww := 0
            signalC(OKR)
```

To ensure given that there are waiting writers, no more than two writers 
will write before a reader is allowed to read the variable `ww` is introduced 
to track the number of times the writers are allowed to proceed the readers.

# Critical Section Problem

<!--
Consider a variation of the Critical Section Problem where there are N processes, and at most
M processes can enter their critical region at the same time, for some M where 1 < M < N .
(We will see later that this provides one way of avoiding deadlock in the Dining Philosophers
Problem.)
-->

## A - Monitors

Program

```
loop                
    m1: Non Critical Section         
    m2: Limit.Enter   
    m3: Crical Section        
    m4: Limit.Leave     
```

Monitor

```
monitor Limit
    int count := 0
    cond ENTER

    op Enter
        if count >= M
            wait(ENTER)
        count := count + 1

    op Leave
        count := count - 1
        signalC(ENTER)
```

### Mutual Exclusion 

No single thread has exclusive access to the critical section
but it is limited to `M` threads at once by counting the number of
threads inside the critical section at once and forcing any extra
threads to wait their turn if the capacity is reached.

### Deadlock

There is no deadlock using the monitor as the monitor does terminate
in both `Enter` and `Leave` operations. The only chance for a deadlock 
would be if `M` is set to be zero or less.

### Starvation

There is no starvation using the monitor assuming that the monitor
will wake the next thread that is waiting in the order of which the
threads arrived in. Thus every thread will eventually be able to take a
step.

## B - Semaphores

// TODO: Semaphores

\pagebreak

# Producers and Consumers revisited

<!--
Consider a variation of the Producers and Consumers problem, where there are M producers
and N consumers, and multiple processes can be writing to or reading from the buffer as
long as they are reading or writing on different locations in the buffer. As before, a producer
grabs a location at the back of the queue to write to and a consumer grabs a location at the
front of the queue to read from, but now they don’t have to wait for previous processes to
finish.
-->

## Mutual Exclusion 

## Deadlock

## Starvation

\pagebreak

# Double Buffering
