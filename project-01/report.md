% NWEN303 Project 1 - Report
% David Barnett (300313764)

# Problem

A group of friends need to get to the next pub which is just beyond a
peculiarly placed maze.

# Solution

## Architecture

The main classes of this program are:

 * `Crawler` which contains the logic for the friends to make
it to the next pub as a group

 * `Square` which represents a tile of the Maze
and,

 * `Maze` class which holds all the `square`s and `crawler`s moving through the Maze.

The main idea behind the architecture is to approach each iteration of the main loop
is one step of a `Crawler`. To make this run in parallel multiple threads run through
the main loop at once. To achieve this the `Maze` object holds a queue
of the next crawler's step to be processed.

A static number of threads are used during the computation of the maze, they are used
as worker threads that will continue to ask the `Maze` if it is done yet and do busy wait's
for new `Crawler`'s to compute.

A common theme to ensure correctness is the usage of Atomic objects with back-offs or
just re-calculating values. This can best be seen in the `move` method of `Crawler` which
bubbles up failures in atomic actions, backs-off and reschedules to try again later if it fails,
otherwise it precedes with confidence that it has successfully recorded its result.

### Crawler

_A group of friend(s) on their way to the pub_

The `Crawler` class represents the friends moving their way through the maze to the next
pub. The major pieces of functionality of the `Crawler` class is the `step` method which
holds all the logic to navigate through the maze.

The main interactions between threads inside a `Crawler` object is at first checking
if another thread is processing the step by checking an `AtomicBoolean`, this is a very basic
non-blocking lock on the object which is very rarely hit due to how the processing queue in the
`Maze` is laid out.

Another major interaction between threads in `Crawler` is during moving between
squares and during splits. When a `Crawler` moves it will check if its destination is occupied,
if it is it will attempt to merge with it by checking if it is currently being processed (if it
is it will back off and check again later as the other crawler is in an undefined state) check if
it they are still there. If we make it through all the pre-conditions we attempt to zero our group's
size (this is done with a compare & swap, and if we noticed our group sized changed we back-off)
and add our group size to the other crawler. After a successful merge the empty crawler will be
removed from the maze given that no one else has merged into in the mean time.

The Crawler objects only care for concurrency with other Crawlers that they run into directly and
only ever merge into other crawlers when they hit them to prevent any Crawler having to second guess
if their group suddenly got stolen, only if they get added to.
This comes into play when a Crawler reaches an intersection where it has to split and walk down
every interaction. To achieve this the crawler repeats attempts at trying to lower its own group size
to a minimum of 1 so it can split up into smaller groups and make new Crawlers to look down other paths. When splitting up the group the original crawler will compute one `move` for each of the
new crawlers and add them to the `Maze`'s queue if they succeed otherwise merges them back into the
original group.

### Square

_just another tile in the Maze_

The `Square` class is a tile of the maze and holds which Crawler is currently on it and
the markings that have been made.
Mutating the `Square`'s markings is only done by the single Crawler that is current on the
square. This is governed by the `AtomicReference<Crawler>` in the Square class which is updated
as a part of the Crawler's method `step`.

### Maze

_The one thing stopping this good night_

The `Maze` hold all the `Square`s that make it up as a grid and also holds the queue of `Crawler`s
to be computed. As well as it independently holds how many people have entered and left the maze
to ensure no one is left behind or find a cloning machine along the way.

The major concurrent component in the `Maze` class is the `crawlersQueue` which holds all the
`Crawler`'s that have steps to be computed. This is filled by checking the result of the `Crawler`'s
`step` method which flags it to say it has more steps to be computed or is at the end / an empty group.
