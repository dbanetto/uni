package org.maze;

import java.io.*;
import java.util.Stack;

public class Runner {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Invalid number of arguments. Expected file path to maze");
        }

        try {
            Maze maze = MazeReader.fromFile(new File(args[0]));
            maze.addCrawler(new Crawler(maze.getStartingPoint(), Direction.West, 100, new Stack<>()));

            do {
                maze.step();
                maze.display();
                System.out.println("Completed Step\n");

            } while (!maze.isComplete());

            maze.display();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
