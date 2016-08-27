package org.maze;

import org.knowm.xchart.BitmapEncoder;
import org.knowm.xchart.XYChart;
import org.knowm.xchart.XYChartBuilder;

import java.io.*;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.*;
import java.util.Stack;
import java.util.stream.Collectors;

public class Runner {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Invalid number of arguments. Expected file path to maze");
        }
        boolean firstRun = true;
        String mazeName = args[0];

        // <Thread#, Time taken with Friends>
        Map<Integer, List<Double>> friendsToTime = new TreeMap<>();
        Set<Integer> threadsData = new HashSet<>();

        for (int threadCount = 1; threadCount <= 32; threadCount++) {
            threadsData.add(threadCount);

            for (int friends = 1; friends < Math.pow(10, 9); friends *= 10) {

                int sampleSize = 100;
                List<Long> samples = new ArrayList<>();

                for (int sample = 0; sample < sampleSize; sample++) {
                    try {
                        // Setup the Map
                        Maze maze = MazeReader.fromFile(new File(mazeName), friends);
                        maze.addCrawler(new Crawler(maze.getStartingPoint(), Direction.West, friends, new Stack<>()));

                        List<Thread> threads = new ArrayList<>();

                        for (int n = 0; n < threadCount; n++) {
                            threads.add(new Thread(() -> {
                                while (!maze.isComplete()) {
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

                double avgTime = (double) (samples.stream().reduce(0L, (a, b) -> a + b)) / (double) sampleSize;
                System.out.printf("Took %fms with to get all %d friends to the pub with %d threads\n", avgTime, friends, threadCount);

                List<Double> times = friendsToTime.get(friends);
                if (times == null) {
                    times = new ArrayList<>();
                    friendsToTime.put(friends, times);
                }
                times.add(avgTime);
            }
        }

        makeTotalChart(mazeName, friendsToTime, threadsData);
        makeTopAndBottomChart(mazeName, friendsToTime, threadsData);
    }

    private static void makeTotalChart(String mazeName, Map<Integer, List<Double>> friendsToTime, Set<Integer> threadsData) {
        String hostname = "localhost";
        try {
            hostname = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }

        XYChart chart = new XYChartBuilder()
                .title("Number of threads vs. number of Friends in " + mazeName + " on " + hostname)
                .xAxisTitle("Number of Threads")
                .yAxisTitle("Time (ms)")
                .width(1000)
                .height(800)
                .build();

        for (Map.Entry<Integer, List<Double>> f : friendsToTime.entrySet()) {
            chart.addSeries(String.format("#%d Friends",
                    f.getKey()),
                    threadsData.stream().collect(Collectors.toList()),
                    f.getValue());
        }

        try {
            BitmapEncoder.saveBitmap(chart,
                    String.format("%s-%s-threads-vs-time", hostname, mazeName),
                    BitmapEncoder.BitmapFormat.PNG);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void makeTopAndBottomChart(String mazeName, Map<Integer, List<Double>> friendsToTime, Set<Integer> threadsData) {
        String hostname = "localhost";
        try {
            hostname = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }

        XYChart chart = new XYChartBuilder()
                .title("First and last of number of threads vs. number of Friends in " + mazeName + " on " + hostname)
                .xAxisTitle("Number of Threads")
                .yAxisTitle("Time (ms)")
                .width(1000)
                .height(800)
                .build();

        Map.Entry<Integer, List<Double>> first = friendsToTime.entrySet()
                .stream()
                .min((a, b) -> a.getKey() - b.getKey()).get();
        Map.Entry<Integer, List<Double>> last = friendsToTime.entrySet()
                .stream()
                .max((a, b) -> a.getKey() - b.getKey()).get();

        chart.addSeries(String.format("#%d Friends",
                first.getKey()),
                threadsData.stream().collect(Collectors.toList()),
                first.getValue());
        chart.addSeries(String.format("#%d Friends",
                last.getKey()),
                threadsData.stream().collect(Collectors.toList()),
                last.getValue());


        try {
            BitmapEncoder.saveBitmap(chart,
                    String.format("%s-%s-threads-vs-time-first-last", hostname, mazeName),
                    BitmapEncoder.BitmapFormat.PNG);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
