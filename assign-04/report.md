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
            self.x = recieve(_, "x")

            # stops the time out from throwing an expection once the time is up
            Timeout.stop()

        catch Timeout as e:
            # just means that there is
            pass

        return self.x

    ...

```

# Question 2

# Question 3

# Question 4
