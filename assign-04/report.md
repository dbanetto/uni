% NWEN303 Assignment 4
% David Barnett (300313764)

# Question 1


The assumption made for these pseudo code programs is that `send` and `revcieve`
are synchronous. As well as it is allowed to have a background thread.
Most decisions are made with a trade of weakly consistency between nodes so
there maybe more extended time or a small period between receiving an update and
applying it.
This was so the rest of the program's correctness is prioritised over having the absolutely
newest information that it could possibly live without.

Below is pseudo code that describes a generic approach to synchronise the value of a
variable.

*Node 5*

```
class Y:

    ...

    # abstraction over the operation to update the value of 'x'
    method setX(value):
        # 
        updateQueue.add('x', value)
        self.x := x
    ...

...

# on a different thread in Node 5 is an event driven
func send_update_loop(targetId, value, variable):

    loop:
        head := updateQueue.pop()
        # iterates over the list of known nodes that are interested by
        # the variable taken from the head of the update queue.
        for targetId in nodePool.for(head.variable):
            send(targetId, head.value, head.variable) 
```

*Node 10*

```
class Y:

    ...

    # abstraction over the operate to get the value of 'x'
    method getX():

        try:
            # set a timer for 10ms, a configurable property
            Timeout.start(10) # start a timer so we do not block for eternity

            # update the value from the one recieved
            # the sender of the update does not matter.
            self.x := recieve(_, "x")

            # stops the time out from throwing an expection once the time is up
            Timeout.stop()

        catch Timeout as e:
            # just means that there is
            pass

        return self.x

    ...

```

Another similar solution would be to have a timer check asynchronously
check for updates to variables.

# Question 2


*Node 0*

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

*Node 1+*

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

# Question 4

*Scatter Gather*

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
