% NWEN303 Assignment 3
% David Barnett (3003133764)

# Question 1

## A)

For a weather forecasting system, a parallel architecture would be the most appropriate.
This is because weather forecasting can be broken down into smaller tasks that can
 run simultaneously.

## B)

For a web server concurrent architecture would be the most appropriate.
This is because there are multiple parts, such as database connections, sending/receiving requests,
accessing cache stores and more that have been designed to be able to handle simultaneous 
requests from many clients

## C)

An appropriate architecture for a airline booking system that checks many fares
would be a distributed architecture.
This is because the booking system will be made up of many different physical machines,
such as one machine dedicated to serving the web application, one to find the airline
fares and another to interact with the booking system securely.

## D)

Latency is the time it take for a single piece of information, such as bits, to travel from
it's source to its destination, the most commonly known form of latency is time between
a client and a remote server in gaming situation.

Bandwidth is the number of pieces of information, such as bits, per unit of
time that can be inputted into a communication channel. A common example
is the bandwidth of a Ethernet network, such as 1GBS (1 Gigabit per second).

## E)

The sequential program takes a total of 24 compute hours.

The parallelized version is ran on 10 nodes for 48 hours, so
the total number of compute hours is $10 * 48 = 480$, 480 hours 
of total compute time.

The speedup is $\frac{T(1)}{T(10)} = \frac{24}{48} = 0.5$.

The speedup is actually a slow down and this could be due the overhead
and setup costs associated with adding more nodes.

# Question 2

A symmetric multiprocessor architecture (SMP) is an architecture which has many processes
sharing a common bus to one or more memory modules on the same physical processing element.

While a non-uniform memory access architecture (NUMA) is a collection of processing units that contain
at least one processing and at least one memory module and a router. Each of these processing
units are connected through a network connected via the routers.

The key difference between the two architectures is a SMP holds all of its memory for it self
while NUMA has its memory distributed over multiple processing units.

# Question 3

The memory access of `P1` to memory location `100` will likely be faster than `P3`'s access to 
to the memory location `210`.

This is because for `P1` the memory location of `100` is stored in `M1` which is in the same processing
element `PE1` allowing it to skip accessing the network which can have a large amount of 
delays relative to being in the same element. 

While `P3`'s request to memory location `210` is stored in the memory unit `M2` which is located in
the processing unit `PE2` while `P3` is located in `PE3`. To complete this request `P3` will have
to access the network via `R3` to `R2` to request for the value at the given memory location.
There is relatively significant overhead and possibilities for deal when compared to access local memory
such as `P1`'s query, these include overheads of sending and receiving between the two routers and
possible delays as the network between them could be in-use by other processing elements, other
delays could be introduced by having to wait for another request to complete.

However `P1`'s access could potentially be slower if it is waiting for requests by other processing units
to be completed before it can complete its request while in the mean time `P3` sends and receives its
requested value.

# Question 4

I would suspect in general the time taken to request a read on a 
NUMA architecture would be faster than a write. 
This is given that the common overhead of accessing the router and network
is the same and the main difference in time coming from how the other processing unit
handles the request.
Since a read is not modifying a resource many can occur at once while a write
may need to block while waiting for other write operations being currently executing
on the same resource. 


# Question 5

The pipeline can start computation can begin once all the workers in the pipeline
has received all their rows and the first worker has been sent to the next worker.
This occurs at `T7` in the lecture slides. The pipeline can begin computation because
`Worker 0` has completed its first task of passing on the column to the next work
so it is clear to start computing a cell of the result matrix.

# Question 6

<!-- 6 rows by 3 columns -->

The pipeline for the dot product of a `6x3` matrix by a `3x3` matrix would mostly be
the same. Each worker could be 
