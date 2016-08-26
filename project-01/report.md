% NWEN303 Project 1 - Report
% David Barnett (300313764)

# Problem

# Solution

## Architecture

The main classes of this program are: 

 * `Crawler` which contains the logic for the friends to make
it to the next pub as a group ; 

 * `Square` which represents a tile of the Maze and the markings
and, ; 

 * `Maze` class which holds all the tiles and crawlers moving through the Maze.

The main idea behind the architecture is to approach each movement as one step and a
thread will compute one step of a crawler at a time. To achieve this the `Maze` object
holds a non-blocking queue from the Java standard library which contains the next crawler to
process. Each thread takes from this shared queue. Crawlers are inserted back into the queue
if they have they still contain some people on the way to their pub.

### Crawler

_Friends that are on their way to the pub_

The `Crawler` class's main functionality is the `step` method which finds the possible
paths which the crawler can take and choses on the path as described in the problem.
This method also handles how the crawler moves with attempting to move to the next tile
and checking if there is already someone there and then attempt to merge with them.

The Crawler has some interaction with other threads.
There are two types for the Crawler, moving through the maze and interacting with 
other Crawlers in the maze.
When the Crawler is moving through the maze uses compare and sets to safely move itself
from one tile to the next using `AtomicReference<T>`'s  

### Square
