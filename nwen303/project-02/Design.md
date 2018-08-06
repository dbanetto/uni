% Design - NWEN 303 - Project 2
% David Barnett (300313764)

# Key manager

## Requirements

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

## Assumptions

 * client will complete the work it is given

# Client Program

## Requirements

* take hostname, port number and chunk size as arguments
* chunk size is the number of keys it will request each time
* attempts to decrypt the cipher text
* report results of keys to Manager

## Assumptions

 * will keep requesting keys until no more tasks available from Key Manager


# Communication between Manger & Client

## Start up

The initial communication is a connect from the client to the manager:

* The manager will give the client the cipher text. 

## Main loop

The main loop of trying keys:

* The client will request for `chunksize` number of keys to attempt.
* The manager will give equal to or less than the requested number of keys.
* The client will work through the keys it received
* The client will send the results of the keys tired with resulting 'plaintext'
* The manager checks the result received, while the client may at anytime ask for more keys

> note: the client can send a result as it reaches the solution or in batches

## Key Found

* The manager will ignore all new requests for keys
* The manager will report the results to standard output


# Message Examples

The messages would be in a binary format to reduce overhead.
This leads to a need for parsing and clearly specifying the layout of messages

## General message layout

The general layout of a message is:

* 1 byte for what type of message: such as 0x00 for initial connection, 0x01 for cipher text passing, etc.

From the first byte the message type is know and passed onto the parser for that message type.

## Startup

The message for initial connection uses the type: Initial Connect (0x00)
