package org.maze;

public enum Mark {
    // Junctions taken and have a dead end
    DEAD,
    // Junctions taken but have not been proven to be dead
    ALIVE,
    // Marks to show the way to the exit
    GOLD,
    // An unmarked wall, could be unvisited or just a regular wall
    NONE,
}
