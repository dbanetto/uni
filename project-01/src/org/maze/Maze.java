package org.maze;

import java.util.*;

public class Maze {
    private final Square[][] grid;
    private List<Crawler> crawlers;
    private List<Crawler> crawlersBuffer;
    private List<Crawler> completed;


    private final Square starting;

    public Maze(Square[][] grid, int startX, int startY) {
        this.grid = grid;
        this.starting = grid[startY][startX];
        crawlersBuffer = new ArrayList<>();
        completed = new ArrayList<>();
    }

    public void addCrawler(Crawler crawler) {
        crawlersBuffer.add(crawler);
    }

    public void addCompletedCrawler(Crawler crawler) {
        completed.add(crawler);
    }

    public void step() {
        // swap crawlers buffers
        crawlers = crawlersBuffer;
        crawlersBuffer = new ArrayList<>(crawlers.size());

        crawlers.forEach(c -> c.step(this));
    }

    public boolean isComplete() {
        return crawlers.stream().allMatch(Crawler::isAtGoal);
    }

    public Square getStartingPoint() {
        return starting;
    }

    public void display() {
        for (int x = 0 ; x < grid.length; x++) {
            for (int y = 0 ; y < grid[x].length; y++) {
                Square square = grid[x][y];
                Optional<Crawler> crawler = getCrawlerAt(square);

                if (crawler.isPresent()) {
                    System.out.print('C');
                } else {
                    System.out.print(square.getTile());
                }
            }
            System.out.print('\n');
        }
    }

    private Optional<Crawler> getCrawlerAt(Square location) {
        return crawlers.stream().filter(c -> c.isAt(location)).findFirst();
    }

}
