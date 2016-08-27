package org.maze;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class Crawler {
    private Square location;
    private Direction from;
    private final AtomicInteger groupSize;
    private final AtomicBoolean inUse;


    private boolean isGolden;
    private final Stack<Direction> pathTaken;

    public Crawler(Square starting, Direction from, int groupSize, Stack<Direction> path) {
        location = starting;
        this.groupSize = new AtomicInteger(groupSize);
        if (groupSize < 1) {
            throw new IllegalArgumentException();
        }
        this.from = from;
        this.inUse = new AtomicBoolean(false);
        pathTaken = (Stack<Direction>) path.clone();
    }

    public boolean goldStep(Maze maze) {
        // System.out.println("Runner:" + pathTaken);
        if (!pathTaken.empty()) {
            Direction next = pathTaken.pop();
            Square before = location;

            if (move(next)) {
                before.getMark(from).set(Mark.GOLD);
                location.getMark(from).set(Mark.GOLD);
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
        if (inUse.get()) {
            return true;
        }

        if (!inUse.compareAndSet(false, true)) {
            return true;
        }

        if (this.isGolden) {
            goldStep(maze);
            inUse.set(false);
            return true;
        }

        if (groupSize.get() < 1) {
            location.getPerson().compareAndSet(this, null);
            inUse.set(false);
            return false;
        }

        List<Direction> possibilities = location.possiblePaths()
                .stream()
                .filter(p -> location.getMark(p).get() != Mark.DEAD)
                .collect(Collectors.toList());

        if (possibilities.size() == 1) {

            Direction moving = possibilities.get(0);
            Square current = location;

            if (move(moving)) {
                current.getMark(moving).compareAndSet(null, Mark.ALIVE);
                location.getMark(from).set(Mark.DEAD);

                if (!pathTaken.isEmpty()) {
                    pathTaken.pop();
                }
                if (!pathTaken.isEmpty()) {
                    pathTaken.pop();
                }
            }

        } else {
            possibilities.remove(from);
            Collections.shuffle(possibilities);

            List<Direction> golden = new ArrayList<>();
            List<Direction> unvisited = new ArrayList<>();
            List<Direction> alive = new ArrayList<>();
            for (Direction possible : possibilities) {
                Mark mark = location.getMark(possible).get();

                if (mark == null) {
                    unvisited.add(possible);
                    continue;
                }

                switch (mark) {
                    case ALIVE:
                        alive.add(possible);
                        break;
                    case GOLD:
                        unvisited.add(possible);
                        break;
                }
            }

            if (!golden.isEmpty()) {
                move(golden.get(0));

            } else if (!unvisited.isEmpty()) {
                splitAtIntersection(maze, unvisited);

            } else if (!alive.isEmpty()) {
                // splitAtIntersection(maze, alive);
                move(alive.get(0));
            }
        }

        if (groupSize.get() < 1) {
            location.getPerson().compareAndSet(this, null);
            inUse.set(false);
            return false;
        }

        if (isAtGoal(maze)) {
            location.getPerson().compareAndSet(this, null);
            maze.completedCrawl(this);
            inUse.set(false);
            return false;
        }
        inUse.set(false);
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
                location.getMark(moving).compareAndSet(null, Mark.ALIVE);
                splitGroup.getLocation().getMark(splitGroup.from).compareAndSet(null, Mark.ALIVE);
                maze.addCrawler(splitGroup);
            } else {
                this.groupSize.addAndGet(split);
            }
        }

        Direction moving = options.remove(0);
        Square current = location;
        if (move(moving)) {
            current.getMark(moving).compareAndSet(null, Mark.ALIVE);
            location.getMark(from).compareAndSet(null, Mark.ALIVE);
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

        if (!isGolden) {
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
            //System.out.println("MERGE!");
            int fromGroupSize;

            fromGroupSize = this.groupSize.get();

            if (mergeTo.inUse.get() || to.get() != mergeTo) {
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

    public AtomicBoolean inUse() { return inUse; }
}
