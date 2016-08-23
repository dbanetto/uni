package org.maze;

import java.io.*;

public class Runner {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Invalid number of arguments. Expected file path to maze");
        }

        try {
            Maze maze = MazeReader.fromFile(new File(args[0]));
            maze.addCrawler(new Crawler(maze.getStartingPoint(), Direction.East, 1));

            do {
                maze.step();
                System.out.println("Completed Step\n");
                maze.display();
            } while (!maze.isComplete());

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
