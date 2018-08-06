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

This monitor works by counting the number of threads that enter and leave
their critical section and limits the number of threads to enter by making the threads
wait if the limit has been reached. The count is decreased as the next thread enters
their critical section.

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

Program

```
loop
    m1: Non Critical Section
    m2: Limit.wait
    m3: Crical Section
    m4: Limit.signal
```

Monitor

```
semphore Limit
    int count := N
    waitset = {}

    op wait
        if count > 0
            count := count - 1
        else
            waitset = waitset + {p}
            p := blocked

    op signal
        if (waitset is empty)
            count := count + 1
        else
           select q from waitset
           waitset := waitset - {q}
           q.state := ready
```
### Mutual Exclusion


### Deadlock

### Starvation

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

Program
---------------------------------------------------------
Producor                      Consumer
----------------------------  ----------------------------
datatype d                    datatype d
loop                          loop
p1: d := produce              c1: f := Buff.enterC
p2: f := Buff.startP          c2: d := f.get
p3: f.set(d)                  c2: Buff.leaveC
p4: Buff.leaveP               c4: consumer(d)
----------------------------------------------------------

Monitor

```
monitor Buff
    array [Future<datatype>] buff := empty
    int p := M
    int c := N
    condtions NotEmpty, NotFull, Last, ConAval, ProdAval

    op startC
        if c > 0
            c := c - 1
        else
            wait(ConAval)

        if buff is empty
            wait(NotEmpty)

        w := dequeue( buff )
        signal(NotFull)
        return w

    op leaveC
        if empty(ConAval)
            c := c + 1
        else
            signal(ConAval)

    op startP
        if p > 0
            p := p - 1
        else
            wait(ProdAval)

        f := new future

        buff := buff.enque(f)
        singal(NotEmpty)

        return f

    op leaveP
        if empty(ProdAval)
            p := p + 1
        else
            signal(ProdAval)

monitor future
    datatype value := None
    conditions filled

    op get
        if value == None
            wait(filled)
        return value

    op set(v)
        if value != None
            value = v
            signal(filled)
        else
            throw error
```

This monitor works by having the `Buff` monitor controls an
array of `future` monitors. To limit the number of producers and consumers at
once both `startP` and `startC` count the number of threads pass their start
and leave operations. Both of the `start` operations return a `future` monitor
which represents the value of one value to be produced and to be consumed. When
the producer enters the critical section it is given a `future` monitor to
store its result from its production and the `Buff` monitor holds a reference
of the monitor in an array. The array of `future` references are handed out,
when available, to consumer threads which when they call `get` the future will
either wait till the producer thread sets the value of the `future` or straight
away returns the result of the producer that was completed earlier.

## Mutual Exclusion

Mutual exclusion of individual elements that has been produced is controlled by
the `future` monitor which ensures that the producer has write exclusion,
following setting the value it then becomes immutable and available to be read
by a consumer. The `future` object is only given out from the `Buff` when there
one available and only gives it to one consumer and one producer.

## Deadlock

Deadlock from entering the `Buff` critical section cannot happen.
This is because the entry counters `c` (for consumers) and `p` (for producers)
will always be greater than zero given no threads are waiting on `ConAval` or
`ProdAval` respectfully, otherwise the exiting thread will wake another thread
to enter the critical section. 
The `future` monitor is free from deadlock as it does not span all threads,
even if it did lock up at most 2 threads 

## Starvation

`Buff` is free from starvation because the conditions that would block a thread
use queues to wake up the next in-line so every thread will eventually get to
take a step.
The `future` monitor is free of starvation because the interdependencies between
`get` and `set` will always resolve as `set` will always execute as apart of
the producers critical section.

\pagebreak

# Double Buffering

Program
---------------------------------------------------------
IO Producer                   IO Consumer
----------------------------  ----------------------------
loop                          loop
p1: d := produce()            c1: d := Double.get
p2: Double.put(d)             c2: consume(d)
----------------------------------------------------------

```
monitor Double
    int r := 0
    int w := N
    buffType read = empty
    buffType write = empty
    cond swap, NotFull

    private op swap
        d := min(N - r, w)
        da := pop d elements from writer

        write := write - da
        read := read + da

        r := r + d
        w := w - d
        signal(swap)
    
    op get
        if r == 0
            if w < N
                swap
            else
                wait(swap)

        r := r - 1
        d := head( read )

        signal(NotFull)

        return d
    
    op put(v)
        if w == 0
            if r < N
                swap
            else
                wait(NotFull)
                swap

        w := w - 1
        write := write.append(v) 

        if !empty(swap)
            swap
```

The `double` monitor works by the consumer only calling `get` to get the 
next element in the read buffer, and the producer calling `put` to insert
a new element into the writer's array. When the consumer runs out of elements in
its buffer it will wait for the producer to swap the buffers when there is at least one
value for it to consume, if the producer has already put some value into the writer buffer
the consumer can cause a buffer swap to not have to wait for the producer to make a new value.
When the producer puts a new value into its buffer it will check if its buffer is full and waits until
the consumer has emptied its buffer or it detected that there is space in the reader buffer and 
inserts as much as it can from its buffer into the reader buffer to give itself more space to produce
more values.

## Mutual Exclusion

There is mutual exclusion of the reader buffer is only accessed by the consumer
thread and the writer buffer can only be accessed by the producer thread. The
only time that both are accessed at once is when a swap occurs which is when
the reader buffer is empty.

## Deadlock

Deadlock does not occur as the producer or consumer can operate independently as long as they
have buffer space or values (respectfully). When they become blocked by one or other the buffer
space or value will eventually become available.

## Starvation

This monitor has a possibility of starvation occurring but it could be called a feature.
When both the reader and writer buffers are filled and the consumer thread is taking forever
to enter its critical section the producer thread will starve as it needs the consumer thread
to take a step.
