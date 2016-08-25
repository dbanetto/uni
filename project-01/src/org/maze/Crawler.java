package org.maze;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.stream.Collectors;

public class Crawler {
    private Square location;
    private Direction from;
    private int groupSize;
    private final Stack<Direction> pathTaken;

    public Crawler(Square starting, Direction from, int groupSize, Stack<Direction> path) {
        location = starting;
        this.groupSize = groupSize;
        if (groupSize < 1) {
            throw new IllegalArgumentException();
        }
        this.from = from;
        pathTaken = (Stack<Direction>)path.clone();
    }

    public void goldStep(Maze maze) {
        System.out.println("Runner:" + pathTaken);
        if (!pathTaken.empty()) {
            Direction next = pathTaken.pop();
            move(next);
            location.getMarks().put(from, Mark.GOLD);
        } else {
            maze.addCrawler(this);
        }
    }

    public void step(Maze maze) {

        List<Direction> possibilities = location.possiblePaths()
                .stream()
                .filter(p -> location.getMarks().get(p) != Mark.DEAD)
                .collect(Collectors.toList());

        if (location.getMarks().get(from) != Mark.DEAD) {
            pathTaken.push(from);
        }

        if (possibilities.size() == 1) {
            Direction moving = possibilities.get(0);

            location.getMarks().put(moving, Mark.ALIVE);
            move(moving);
            location.getMarks().put(from, Mark.DEAD);
            if (!pathTaken.isEmpty()) {
                pathTaken.pop();
            }
            attemptMerge(maze);

        } else {
            possibilities.remove(from);

            List<Direction> golden = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.GOLD).collect(Collectors.toList());
            List<Direction> unvisited = possibilities.stream().filter(p -> location.getMarks().get(p) == null).collect(Collectors.toList());
            List<Direction> alive = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.ALIVE).collect(Collectors.toList());

            if (!golden.isEmpty()) {
                move(golden.get(0));
                attemptMerge(maze);
            } else if (!unvisited.isEmpty()) {
                splitAtIntersection(maze, unvisited);

            } else if (!alive.isEmpty()) {
                splitAtIntersection(maze, alive);
            }
        }


        if (!isAtGoal()) {
            // let 0 zero groups die out
            if (this.groupSize > 0) {
                maze.addCrawler(this);
            }
        } else {
            maze.addCompletedCrawler(this);
            // won't be taking next step so store it now
            pathTaken.push(from);
        }
    }

    private void splitAtIntersection(Maze maze, List<Direction> options) {

        // groupSize >= options
        int groups = Math.min(groupSize, options.size());
        int split = Math.round((float)groupSize / (float)groups);

        for (int i = 1; i < groups; i++) {
            Direction moving = options.remove(0);
            Crawler splitGroup = new Crawler(location, from, split, pathTaken);

            location.getMarks().putIfAbsent(moving, Mark.ALIVE);
            splitGroup.move(moving);
            location.getMarks().putIfAbsent(splitGroup.from, Mark.ALIVE);

            if (!attemptMerge(maze)) {
                maze.addCrawler(splitGroup);
            }
        }

        Direction moving = options.remove(0);
        location.getMarks().putIfAbsent(moving, Mark.ALIVE);
        move(moving);
        location.getMarks().putIfAbsent(from, Mark.ALIVE);

        groupSize = groupSize - (split * (groups - 1));
        attemptMerge(maze);
    }

    private boolean attemptMerge(Maze maze) {
        Optional<Crawler> collision = maze.getCrawlerAt(location, this);

        if (collision.isPresent()) {
            Crawler merged = collision.get();
            merged.groupSize += this.groupSize;
            this.groupSize = 0;
            System.out.println("Merged with another group!");
            return true;
        }
        return false;
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

    @Override
    public String toString() {
        return "Crawler{" +
                "from=" + from +
                ", groupSize=" + groupSize +
                '}';
    }
}
