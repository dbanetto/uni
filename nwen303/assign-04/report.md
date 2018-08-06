% NWEN303 Assignment 4
% David Barnett (300313764)

# Question 1

### Node 5

```
func set_x(new_x):
    self.x = new_x
    send(10, 'x', new_x)
```

On an update of the variable `x` node 5 sends a message to node 10 to inform
it of the change of value.

### Node 10

```
func get_x():
    receive(5, 'x', &self.x)
    return self.x
```

`receive` update the variable `x` from the message received from node 5 

The main draw backs of such a simple version of this algorithms is that
if `send` and/or `recieve` are blocking either one or both of the nodes will
block and wait for the other node to do their related operation.

# Question 2

The assumption of this program is `recieve` will return a truth value if 
it has received a message from a node and will consume the message.
The other assumption made is that `movement_detected` blocks until movement is
detected by the node.

### Node 0

```
loop:

    for node in nodes:
       
       # returns true if something has been recieved
       if revcieve(node):
            # got a message we can assume it is a detection as it is the
            # only message being passed by the nodes
            flash(node.room_id)
```

Node 0 continually polls the other nodes for an event and shows a flash when one
is received.

### Node 1+

```
loop:
    # blocks until a movement is detected
    movement_detected()

    # asynchronous send
    send(this.room_id)

```

Nodes 1+ block their loop until they are interrupted with a detection event
and send a message to coordinator to inform them about the event.

# Question 3

We would need a differing implementation of a program from running on N nodes
than to a serial version. This is due to the differences of design of the two
implementations where each implementation focuses on different aspects and have
different considerations. For example the serial implementation would have gain
the most from being memory efficient to get the most gains, while a distributed
implementation would focus more on the communication between nodes to shave off
the most significant overhead.
From these differences in design if serial was to use a 1 node variant of the parallel
implementation the comparison would be unfair as it includes the overhead that is associated
with performing over many nodes.
To make the comparison fair for the speedup the best and most appropriate implementations
need to be used.

# Question 4

### Scatter Gather

```
# array is an array of size N
# nodes is an array of worker node id's of size S
func scatter_gather(array, nodes):

    # evenly size up the task for every node
    # assuming that every node has the amount of compute to use on this task
    chunk := len(array) / len(nodes)

    # keep track of the current index being chuncked up
    i := 0

    # send the chunck to every node
    for node in nodes:
        # slice up the array into a chunk
        send(node, array.slice(i, i + chunk)

        i := i + chunk

    result := []

    # collate the results
    for node in nodes:
        # blocks on recieve till the node returns back the result
        sub_result = recieve(node)

        result.append(sub_result)

    return result
```

This problem fits the description of a embarrassingly parallel problem.
Which is a problem that requires very little or no effort to parallelize the
problem. This problem is embarrassingly parallel because the only work the coordinator
needs to do to make the problem parallel is to chunk up the size of the array and send it
other nodes, followed up receiving the results back and merging them together.
The main complexity to making this problem parallel is the mechanics to communicate between
many nodes and give guarantees that a chunk of work will be completed by the node that was
assigned the work. These complexities have been assumed to be handled by the underlying 
message passing protocol.
