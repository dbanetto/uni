% NWEN303 - Project 2
% David Barnett (300313764)

# Task 1

## Changes to the echo server / client.

The echo server's joke was changed to:

> Knock, Knock
> Justin
> Justin Who?
> Just in the neighbourhood, thought I would drop by.

To achieve these changes I changed the sequence of message and
the  replies printed in `EchoClient.java` and also updated the
message checks and replies in `Connection.java`.

## Evidence of the program function

### EchoServer

```
Waiting for connections on 43501
Client 1: Knock, knock
Server: Who's there?
Client 1: Justin
Server: Justin who?
Client 1: Just in the neighborhood, thought I would drop by.
Server: <<<<groan>>>>
Client 1: bye
```

### EchoClient

```
Who's there?
Justin who?
<<<<groan>>>>
```

## Changes to the Search.

To achieve a user defined amount of keys to try the number is captured from the
arguments of the program.

The argument list was changed from:

> Search <starting key> <key size> <ciphertext>

To:

> Search <starting key> <key size> <number to try> <ciphertext>

Using the <number to try> instead of the constant 100 in the loop of different
keys to try.

## Evidence of Program's function:

**note**: non-printable characters have been omitted with a `...`

The command below gave the following output.

> Search 3185209670 4 1 :+UHC88LxQEgKq6BmdGo31UtE5HqTimlZssAZMXqSXXXT7NJLc52Fng==

```
3185209670...No key found!
```

The command below gave the following output.

> Search 3185209670 4 100 :+UHC88LxQEgKq6BmdGo31UtE5HqTimlZssAZMXqSXXXT7NJLc52Fng==

```
3185209670...3185209671...3185209672...3185209673...3185209674...3185209675...
3185209676...3185209677...3185209678...3185209679...3185209680...Plaintext found!

May good flourish; Kia hua ko te pai
```

\pagebreak

# Task 2 - Design

<!-- TODO: here -->
## Key manager

<!-- TODO: here -->
### Requirements

* take initial key, key size and cipher text as arguments
* host on a random port
* Connections from a client
* allocate them key space to search
* report results
* Answer requests for work
* Receive results from a client
* Shutdown when the key is found
* print out if it found key & (given found) the key. Otherwise print failure
* track time taken and the key space exhausted

<!-- TODO: here -->
### Assumptions

 * client will complete the work it is given

## Client Program

<!-- TODO: here -->
### Requirements

* take hostname, port number and chunk size as arguments
* chunk size is the number of keys it will request each time
* attempts to decrypt the cipher text
* report results of keys to Manager

<!-- TODO: here -->
### Assumptions

 * will keep requesting keys until no more tasks available from Key Manager
 * will not crash while completing task
 * the key manager will be accepting requests if and only if the key has not been found yet

## Communication between Manger & Client

<!-- TODO: here -->
### Start up

<!-- TODO: here -->
The initial communication is a connect from the client to the manager:

* The manager will give the client the cipher text. 

### Main loop

The main loop of trying keys:

<!-- TODO: here -->
* The client will request for `chunksize` number of keys to attempt.
* The manager will give equal to or less than the requested number of keys.
* The client will work through the keys it received
* The client will send the results of the keys tired, either reporting failure on a set of keys or success on a single key
* The manager checks the result received for success or failure

> note: the client can send a result as it reaches the solution or in batches

### Key Found

## Message Examples

The messages would be in a binary format to reduce overhead.
This leads to a need for parsing and clearly specifying the layout of messages

### General message layout

The general layout of a message is:

* 1 byte for what type of message: such as 0x00 for initial connection, 0x01 for cipher text passing, etc.

From the first byte the message type is know and passed onto the parser for that message type.

### Startup

<!-- TODO: here -->


## Implementation meets requirements

### Key Manager

### Client
