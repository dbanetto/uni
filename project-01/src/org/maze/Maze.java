package org.maze;

import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class Maze {
    private final Square[][] grid;

    private Queue<Crawler> crawlerQueue;

    private final Square starting;
    private final int groupSize;
    private int exited;
    private final List<Square> exits;
    private final Map<Square, Seeker> exitGolden;

    public Maze(Square[][] grid, int groupSize, int startX, int startY) {
        this.grid = grid;
        this.starting = grid[startY][startX];
        this.exits = buildFinishPoints();
        this.groupSize = groupSize;
        exitGolden = new HashMap<>();

        crawlerQueue = new LinkedBlockingQueue<>();
    }


    public synchronized void addCrawler(Crawler crawler) {
        crawlerQueue.add(crawler);
    }

    public void completedCrawl(Crawler crawler) {
        if (!exitGolden.containsKey(crawler.getLocation())) {

            Seeker out = exitGolden.put(crawler.getLocation(), new Seeker(crawler));
            if (out != null) {
                System.out.print("m8");
            }
            crawler.setGolden(true);
            addCrawler(crawler);
        } else {
            this.exited = exits.stream().mapToInt(e -> {
                AtomicReference<Crawler> person = e.getPerson();
                if (person.get() != null) {
                    return person.get().getGroupSize().get();
                }
                return 0;
            }).sum();
        }
    }

    public void completedGoldCrawl(Crawler crawler) {
        Map.Entry<Square, Seeker> seeker = exitGolden.entrySet().stream()
                .filter(e -> e.getValue().crawler == crawler)
                .findFirst().get();
        seeker.getValue().complete = true;

        exitGolden.put(seeker.getKey(), seeker.getValue());
    }

    public boolean isComplete() {
        return exited == groupSize;
    }

    public Square getStartingPoint() {
        return starting;
    }

    public List<Square> getExits() {
        return exits;
    }

    private List<Square> buildFinishPoints() {
        List<Square> finishes = new ArrayList<>();
        Square startingPoint = getStartingPoint();

        // Check top line
        int staticY = 0;
        for (int x = 0; x < grid[staticY].length; x++) {
            if (grid[staticY][x] != startingPoint) {
                finishes.add(grid[staticY][x]);
            }
        }

        // Check bottom line
        staticY = grid.length - 1;
        for (int x = 0; x < grid[staticY].length; x++) {
            if (grid[staticY][x] != startingPoint) {
                finishes.add(grid[staticY][x]);
            }
        }

        // check left side
        int staticX = 0;
        for (int y = 0; y < grid.length; y++) {
            if (grid[y][staticX] != startingPoint) {
                finishes.add(grid[y][staticX]);
            }
        }

        // check right side
        staticX = grid[0].length - 1;
        for (int y = 0; y < grid.length; y++) {
            if (grid[y][staticX] != startingPoint) {
                finishes.add(grid[y][staticX]);
            }
        }

        return finishes.stream().filter(p -> p.getTile() == Tile.SPACE).collect(Collectors.toList());
    }

    public synchronized void display() {
        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                Square square = grid[y][x];

                System.out.print(square);
            }
            System.out.print('\n');
        }
    }

    public synchronized List<Crawler> findWanders() {
        List<Crawler> wanders = new ArrayList<>();
        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                Square square = grid[y][x];

                Crawler wander = square.getPerson().get();
                if (wander != null && !exits.contains(square)) {
                    if (wander.getGroupSize().get() > 0) {
                        wanders.add(wander);
                    } else {
                        square.getPerson().compareAndSet(wander, null);
                    }
                }
            }
        }
        return wanders;
    }

    public void step() {
        Crawler crawler = crawlerQueue.poll();
        if (crawler != null && crawler.step(this)) {
            crawlerQueue.add(crawler);
        }
    }

    private class Seeker {
        final Crawler crawler;
        boolean complete = false;

        public Seeker(Crawler crawler) {
            this.crawler = crawler;
        }
    }
}
