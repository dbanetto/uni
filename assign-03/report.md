% NWEN303 Assignment 3
% David Barnett (3003133764)

# Question 1

## A)

## B)

## C)

## D)

## E)

The sequential program takes a total of 24 compute hours.

The parallelized version is ran on 10 nodes for 48 hours, so
the total number of compute hours is $10 * 48 = 480$, 480 hours 
of total compute time.

The speedup is the percentage increase between sequential and parallelized versions
of the program. In this case it would be $\frac{480}{24} = 20$, a 20 times increase in the
total time to calculate. Thus the total speed up from parallelizing the program resulted
in a 20 times slower program.


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

# Question 5

# Question 6
