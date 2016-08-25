package org.maze;

import java.util.*;

public class Maze {
    private final Square[][] grid;
    private List<Crawler> crawlers;
    private List<Crawler> crawlersBuffer;

    private List<Crawler> completed;

    private boolean runComplete = false;
    private Crawler goldenRunner;


    private final Square starting;

    public Maze(Square[][] grid, int startX, int startY) {
        this.grid = grid;
        this.starting = grid[startY][startX];
        crawlersBuffer = new ArrayList<>();
        completed = new ArrayList<>();
    }

    public void addCrawler(Crawler crawler) {
        if (goldenRunner == crawler) {
            runComplete = true;
            goldenRunner = null;
        }
        crawlersBuffer.add(crawler);
    }

    public void addCompletedCrawler(Crawler crawler) {
        if (goldenRunner == null && !runComplete) {
            goldenRunner = crawler;
        } else {
            completed.add(crawler);
        }
    }

    public void step() {
        // swap crawlers buffers
        crawlers = crawlersBuffer;
        crawlersBuffer = new ArrayList<>(crawlers.size());

        if (goldenRunner != null) {
            goldenRunner.goldStep(this);
        }
        crawlers.forEach(c -> c.step(this));
    }

    public boolean isComplete() {
        return crawlersBuffer.isEmpty() && !completed.isEmpty();
    }

    public Square getStartingPoint() {
        return starting;
    }

    public void display() {
        for (int x = 0 ; x < grid.length; x++) {
            for (int y = 0 ; y < grid[x].length; y++) {
                Square square = grid[x][y];
                Optional<Crawler> crawler = getCrawlerAt(square, null);
                Optional<Crawler> complete = completed.stream().filter(c -> c.isAt(square)).findFirst();

                if (crawler.isPresent()) {
                    System.out.print('C');
                } else if (goldenRunner != null && goldenRunner.isAt(square)) {
                    System.out.print('G');
                } else if (complete.isPresent()) {
                    System.out.print('P');
                } else {
                    System.out.print(square.getTile());
                }
            }
            System.out.print('\n');
        }
    }

    public Optional<Crawler> getCrawlerAt(Square location, Crawler not) {
        return crawlers.stream().filter(c -> c != not && c.isAt(location)).findFirst();
    }

}
