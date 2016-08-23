package org.maze;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Crawler {
    private Square location;
    private Direction from;
    private int groupSize;

    public Crawler(Square starting, Direction from, int groupSize) {
        location = starting;
        this.groupSize = groupSize;
        if (groupSize < 1) {
            throw new IllegalArgumentException();
        }
        this.from = from;
    }

    public void step(Maze maze) {

        List<Direction> possibilities = location.possiblePaths()
                .stream()
                .filter(p -> location.getMarks().get(p) != Mark.DEAD)
                .collect(Collectors.toList());

        if (possibilities.size() == 1) {
            Direction moving = possibilities.get(0);

            location.getMarks().put(moving, Mark.ALIVE);
            move(moving);
            location.getMarks().put(from, Mark.DEAD);

        } else {
            possibilities.remove(from);

            List<Direction> golden = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.GOLD).collect(Collectors.toList());
            List<Direction> unvisited = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.NONE).collect(Collectors.toList());
            List<Direction> alive = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.ALIVE).collect(Collectors.toList());

            if (!golden.isEmpty()) {
                move(golden.get(0));
            } else if (!unvisited.isEmpty()) {
                Direction moving = unvisited.remove(0);

                location.getMarks().put(moving, Mark.ALIVE);
                move(moving);
                location.getMarks().put(from, Mark.ALIVE);

            } else if (!alive.isEmpty()) {
                Direction moving = alive.remove(0);

                move(moving);
            }
        }


        if (!isAtGoal()) {
            maze.addCrawler(this);
        } else {
            maze.addCompletedCrawler(this);
        }
    }

    private void splitAtIntersection(Maze maze, List<Direction> options) {

    }

    public boolean isAtGoal() {
        return Arrays.stream(Direction.values()).anyMatch(d -> location.getNeighbour(d) == null);
    }

    private void move(Direction direction) {
        System.out.println(this + " moved " + direction);

        from = direction.turnAround();
        location = location.getNeighbour(direction);

        if (location == null) {
            throw new IllegalStateException("Moved to null tile");
        }
    }

    public boolean isAt(Square location) {
        return location.equals(this.location);
    }
}
