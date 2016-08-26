package org.maze;

import java.util.*;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class Maze {
    private final Square[][] grid;

    private Queue<Crawler> crawlerQueue;

    private final Square starting;
    private final int groupSize;
    private final AtomicInteger exited;
    private final List<Square> exits;
    private final Map<Square, Seeker> exitGolden;

    public Maze(Square[][] grid, int groupSize, int startX, int startY) {
        this.grid = grid;
        this.starting = grid[startY][startX];
        this.exits = buildFinishPoints();
        this.groupSize = groupSize;
        this.exitGolden = new HashMap<>(exits.size());
        this.exited = new AtomicInteger(0);

        this.exits.forEach(e -> exitGolden.put(e, new Seeker()));

        crawlerQueue = new ConcurrentLinkedQueue<>();
    }


    public void addCrawler(Crawler crawler) {
        crawlerQueue.offer(crawler);
    }

    public void completedCrawl(Crawler crawler) {
        Square exitPoint = crawler.getLocation();
        Seeker exitSeeker = exitGolden.get(exitPoint);

        if (exitSeeker.crawler.compareAndSet(null, crawler)) {
            crawler.setGolden(true);
            addCrawler(crawler);
            return;
        }

        this.exited.addAndGet(crawler.getGroupSize().get());
    }

    public void completedGoldCrawl(Crawler crawler) {
        Optional<Map.Entry<Square, Seeker>> isSeeker = exitGolden.entrySet().stream()
                .filter(e -> e.getValue().crawler.get() == crawler)
                .findFirst();

        if (isSeeker.isPresent()) {
            Map.Entry<Square, Seeker> seeker = isSeeker.get();
            seeker.getValue().complete = true;
            exitGolden.put(seeker.getKey(), seeker.getValue());
        }
    }

    public boolean isComplete() {
        return exited.get() == groupSize;
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


    public void step() {
        Crawler crawler = crawlerQueue.poll();
        if (crawler != null && crawler.step(this)) {
            crawlerQueue.add(crawler);
        }
    }

    private class Seeker {
        final AtomicReference<Crawler> crawler;
        boolean complete = false;

        public Seeker() {
            this.crawler = new AtomicReference<>(null);
        }
    }

    // Utility Function which is good for debugging
    public void display() {
        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                Square square = grid[y][x];

                System.out.print(square);
            }
            System.out.print('\n');
        }
    }

    // Utility Function which is good for debugging
    public List<Crawler> findWanders() {
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
}
