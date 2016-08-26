package org.maze;

import java.io.*;
import java.util.*;
import java.util.Stack;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class Runner {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Invalid number of arguments. Expected file path to maze");
        }
        boolean firstRun = true;

        for (int i = 10; i < Math.pow(10, 8); i *= 10) {
            int friends = i;

            int sampleSize = 100;
            List<Long> samples = new ArrayList<>();

            for (int sample = 0; sample < sampleSize; sample++) {
                try {
                    Maze maze = MazeReader.fromFile(new File(args[0]), friends);
                    maze.addCrawler(new Crawler(maze.getStartingPoint(), Direction.West, friends, new Stack<>()));

                    List<Thread> threads = new ArrayList<>();

                    for (int n = 0; n < 2; n++) {
                        threads.add(new Thread(() -> {
                            while(!maze.isComplete()) {
                                maze.step();
                                // maze.display();
                            }
                        }));
                    }

                    long startTime = System.currentTimeMillis();

                    threads.forEach(Thread::start);
                    for (Thread t : threads) {
                        t.join();
                    }

                    long endTime = System.currentTimeMillis();

                    // ignore the result of the 1st run as the JVM has not warmed up
                    if (firstRun) {
                        sample--;
                        firstRun = false;
                        continue;
                    }
                    // maze.display();

                    samples.add(endTime - startTime);

                } catch (IOException e) {
                    e.printStackTrace();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                System.out.print(".");
            }
            System.out.print("\n");

            double avgTime = (double)(samples.stream().reduce(0L, (a, b) -> a + b )) / (double)sampleSize;
            System.out.printf("Took %fms with to get all %d friends to the pub\n", avgTime, friends);
        }
    }
}
