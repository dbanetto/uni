package org.maze;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class Crawler {
    private Square location;
    private Direction from;
    private AtomicInteger groupSize;
    private boolean inUse = false;


    private boolean isGolden;
    private final Stack<Direction> pathTaken;

    public Crawler(Square starting, Direction from, int groupSize, Stack<Direction> path) {
        location = starting;
        this.groupSize = new AtomicInteger(groupSize);
        if (groupSize < 1) {
            throw new IllegalArgumentException();
        }
        this.from = from;
        pathTaken = (Stack<Direction>) path.clone();
    }

    public boolean goldStep(Maze maze) {
        // System.out.println("Runner:" + pathTaken);
        if (!pathTaken.empty()) {
            Direction next = pathTaken.pop();
            Square before = location;

            if (move(next)) {
                before.getMarks().put(from, Mark.GOLD);
                location.getMarks().put(from, Mark.GOLD);
            } else {
                pathTaken.push(next);
            }

        } else {
            maze.completedGoldCrawl(this);
            this.isGolden = false;
        }
        return true;
    }

    public boolean step(Maze maze) {
        if (inUse) {
            return false;
        }

        inUse = true;
        if (this.isGolden) {
            goldStep(maze);
            inUse = false;
            return true;
        }

        if (groupSize.get() < 1) {
            location.getPerson().compareAndSet(this, null);
            inUse = false;
            return false;
        }

        List<Direction> possibilities = location.possiblePaths()
                .stream()
                .filter(p -> location.getMarks().get(p) != Mark.DEAD)
                .collect(Collectors.toList());

        if (possibilities.size() == 1) {

            Direction moving = possibilities.get(0);
            Square current = location;

            if (move(moving)) {
                current.getMarks().put(moving, Mark.ALIVE);
                location.getMarks().put(from, Mark.DEAD);

                if (!pathTaken.isEmpty()) {
                    pathTaken.pop();
                }
            }

        } else {
            possibilities.remove(from);

            List<Direction> golden = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.GOLD).collect(Collectors.toList());
            List<Direction> unvisited = possibilities.stream().filter(p -> location.getMarks().get(p) == null).collect(Collectors.toList());
            List<Direction> alive = possibilities.stream().filter(p -> location.getMarks().get(p) == Mark.ALIVE).collect(Collectors.toList());

            if (!golden.isEmpty()) {
                move(golden.get(0));

            } else if (!unvisited.isEmpty()) {
                splitAtIntersection(maze, unvisited);

            } else if (!alive.isEmpty()) {
                splitAtIntersection(maze, alive);

            }

        }

        inUse = false;
        if (isAtGoal(maze)) {
            maze.completedCrawl(this);

            return false;
        }
        return true;
    }

    private void splitAtIntersection(Maze maze, List<Direction> options) {
        int groupSize, groups, split, newGroupSize;

        do {
            groupSize = this.groupSize.get();
            groups = Math.min(groupSize, options.size());
            split = Math.round((float) groupSize / (float) groups);

            newGroupSize = groupSize - (split * (groups - 1));

        } while (!this.groupSize.compareAndSet(groupSize, newGroupSize));

        assert (split > 0 || groupSize > 0);

        for (int i = 1; i < groups; i++) {
            Direction moving = options.remove(0);
            Crawler splitGroup = new Crawler(location, from, split, pathTaken);

            if (splitGroup.move(moving)) {
                location.getMarks().putIfAbsent(moving, Mark.ALIVE);
                splitGroup.getLocation().getMarks().putIfAbsent(splitGroup.from, Mark.ALIVE);
            }

            maze.addCrawler(splitGroup);
        }

        Direction moving = options.remove(0);
        Square current = location;
        if (move(moving)) {
            current.getMarks().putIfAbsent(moving, Mark.ALIVE);
            location.getMarks().putIfAbsent(from, Mark.ALIVE);
        }
    }

    public boolean isAtGoal(Maze maze) {
        return maze.getExits().stream().anyMatch(this::isAt);
    }

    private boolean move(Direction direction) {
        //System.out.println(this + " moved " + direction);

        Square movingTo = location.getNeighbour(direction);
        Square current = location;

        if (!attemptMerge(current.getPerson(), movingTo.getPerson()))  {
            return false;
        }

        location = movingTo;
        from = direction.turnAround();

        if (location.getMarks().get(from) != Mark.DEAD && !isGolden) {
            pathTaken.push(from);
        }
        return true;
    }

    private boolean attemptMerge(AtomicReference<Crawler> from, AtomicReference<Crawler> to) {
        Crawler mergeTo = to.get();

        if (mergeTo == null) {
            if (!to.compareAndSet(null, this)) {
                return false;
            }
            //System.out.println("Smooth sailing");
        } else if (this.isGolden) {
            //System.out.println("GOLD!");
            return false;
        } else {
            if (mergeTo.inUse) {
                return false;
            }

            //System.out.println("MERGE!");
            int toGroupSize, mergedGroupSize, fromGroupSize, total;

            toGroupSize = mergeTo.getGroupSize().get();
            fromGroupSize = this.groupSize.get();

            mergedGroupSize = toGroupSize + fromGroupSize;

            if (to.get() != mergeTo) {
                return false;
            }
            if (!this.groupSize.compareAndSet(fromGroupSize, 0)) {
                return false;
            }
            mergeTo.getGroupSize().addAndGet(fromGroupSize);

            //System.out.println("MERGE COMPLETE!");
        }

        from.compareAndSet(this, null);
        return true;
    }

    public boolean isAt(Square location) {
        return location.equals(this.location);
    }

    public boolean isGolden() {
        return isGolden;
    }

    public void setGolden(boolean golden) {
        isGolden = golden;
    }

    public Square getLocation() {
        return location;
    }

    public AtomicInteger getGroupSize() {
        return groupSize;
    }
}
