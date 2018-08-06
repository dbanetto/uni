import java.io.IOException;
import java.net.InetAddress;
import java.util.*;
import org.knowm.xchart.BitmapEncoder;
import org.knowm.xchart.XYChart;
import org.knowm.xchart.XYChartBuilder;
import org.knowm.xchart.XYSeries;

/**
 * Sum an array, using threads, with timing
 * Create an array and sum it using several threads and time.
 * <p>
 * This puts the sum in a static so the threads can share it.
 * Also uses a static counter to generate thread id's - or we can use default
 * names given by getName().
 * <p>
 * Uses this.yield() to mix it up!
 * <p>
 * This version optionally takes number of threads and array size as
 * arguments; defaults are 4 and 100.
 */

public class ArraySum {

    static int N = 4;              // Number of threads
    static int M = 100;            // Array size
    static int x[] = new int[M];

    static Object lock = new Object();
    static int sum;

    // Sum without threads

    private static List<Long>  Sum0() {

        StopWatch w = new StopWatch();
        w.start();

        int sum = 0;
        for (int i = 0; i < M; i++) {
            sum = sum + x[i];
            // Thread.currentThread().interrupt();
        }

        w.stop();

        // System.out.println(String.format("single,%d,%d,%d,%d", N, M, w.getElapsedTime(), 0));
        List<Long> results = new ArrayList<>();
        results.add(w.getElapsedTime());
        results.add(0L);
        return results;
    }

    // Sum with threads
    private static List<Long> SumN(int N) {
        StopWatch w = new StopWatch();
        StopWatch threads = new StopWatch();
        w.start();

        sum = 0;

        threads.start();
        Adder[] adder = new Adder[N];

        int chunkSize = M / N;    // Size of chunk given to each process

        // Give chunkSize to the first N-1 threads
        for (int i = 0; i < N - 1; i++) {
            adder[i] = new Adder(x, i * chunkSize, (i + 1) * chunkSize - 1);
        }

        // Give the rest to the last thread
        adder[N - 1] = new Adder(x, (N - 1) * chunkSize, M - 1);

        // Now start them
        for (int i = 0; i < N; i++) {
            adder[i].start();
        }
        threads.stop();

        // Wait till they're all finished, then print a message

        try {
            for (int k = 0; k < N; k++) {
                adder[k].join();
            }
        } catch (InterruptedException e) {
            System.out.println("Ouch");
        }

        w.stop();
        // System.out.println(String.format("global,%d,%d,%d,%d", N, M, w.getElapsedTime(), threads.getElapsedTime()));

        List<Long> results = new ArrayList<>();
        results.add(w.getElapsedTime());
        results.add(threads.getElapsedTime());
        return results;
    }

    // Sum with threads with Local
    private static List<Long> SumNLocal(int N) {
        StopWatch w = new StopWatch();
        StopWatch threads = new StopWatch();
        w.start();

        int localSum = 0;

        threads.start();
        AdderLocal[] adder = new AdderLocal[N];

        int chunkSize = M / N;    // Size of chunk given to each process

        // Give chunkSize to the first N-1 threads
        for (int i = 0; i < N - 1; i++) {
            adder[i] = new AdderLocal(x, i * chunkSize, (i + 1) * chunkSize - 1);
        }

        // Give the rest to the last thread
        adder[N - 1] = new AdderLocal(x, (N - 1) * chunkSize, M - 1);

        // Now start them
        for (int i = 0; i < N; i++) {
            adder[i].start();
        }
        threads.stop();

        // Wait till they're all finished, then print a message

        try {
            for (int k = 0; k < N; k++) {
                AdderLocal current = adder[k];
                current.join();
                localSum += current.sum;
            }
        } catch (InterruptedException e) {
            System.err.println("Ouch");
        }
        w.stop();

        // System.out.println(String.format("local,%d,%d,%d,%d", N, M, w.getElapsedTime(), threads.getElapsedTime()));

        List<Long> results = new ArrayList<>();
        results.add(w.getElapsedTime());
        results.add(threads.getElapsedTime());
        return results;
    }

    // Sum with threads with Local
    private static List<Long> SumNRec() {
        StopWatch w = new StopWatch();
        StopWatch threads = new StopWatch();
        w.start();

        int localSum = 0;

        threads.start();
        AdderRec adder;

        adder = new AdderRec(x, 0, M - 1);

        // Now start them
        adder.start();
        threads.stop();

        // Wait till they're all finished, then print a message

        try {
            adder.join();
            localSum += adder.sum;
        } catch (InterruptedException e) {
            System.err.println("Ouch");
        }
        w.stop();

        List<Long> results = new ArrayList<>();
        results.add(w.getElapsedTime());
        results.add(threads.getElapsedTime());
        return results;
    }

    // Sum with threads with Local
    private static List<Long> SumStream() {
        StopWatch w = new StopWatch();
        w.start();

        // Wait till they're all finished, then print a message
        // Arrays.asList(x).parallelStream().mapToInt(value -> value).sum();

        w.stop();

        List<Long> results = new ArrayList<>();
        results.add(w.getElapsedTime());
        results.add(0L);
        return results;
    }


    public static void main(String[] args) {
        String method = "seq";
        for (int sizePower = 7; sizePower < 8; sizePower++) {
            int size = 10000000;

            List<Double> avgTotalTime = new ArrayList<>();
            List<Double> avgCreateTime = new ArrayList<>();
            List<Integer> threads = new ArrayList<>();

            System.out.println(String.format("Using array size of %d", size));
            for (int thread = (size / 100); thread <= size; thread += (size / 100)) {
                N = thread;
                M = thread;
                AdderRec.Threashold = thread;
                System.out.println(String.format("Using %d threads", thread));

                List<Long> totalTime = new ArrayList<>();
                List<Long> createTime = new ArrayList<>();

                // Create and initialise array to 0..M-1.

                x = new int[M];
                for (int i = 0; i < M; i++) x[i] = i + 1;

                // Sum0();

                for (int attempt = 0; attempt < 10; attempt++) {
                    List<Long> results = Sum0();
                    // List<Long> results = SumN(N);
                    // List<Long> results = SumNLocal(N);
                    // List<Long> results = SumNRec();

                    totalTime.add(results.get(0));
                    createTime.add(results.get(1));
                }
                avgTotalTime.add(average(totalTime));
                avgCreateTime.add(average(createTime));


                threads.add(thread);
            }

            XYChart chart = new XYChartBuilder()
                    .xAxisTitle("Array Size")
                    .yAxisTitle("Avg Time")
                    .width(800)
                    .height(800)
                    .build();

            XYSeries series1 = chart.addSeries("Total Time", threads, avgTotalTime);
            // XYSeries series2 = chart.addSeries("Create Time", threads, avgCreateTime);

            try {
                BitmapEncoder.saveBitmap(chart,
                        String.format("./%s-%s-threads-vs-time-%d",
                                InetAddress.getLocalHost().getHostName(), method , size),
                        BitmapEncoder.BitmapFormat.PNG);
                System.out.println(String.format("Made graph: threads-vs-time-%d.png", size));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        SumNRec();

    }

    static double average(List<Long> data) {
        Long total = 0L;
        for (Long i : data) {
            total += i;
        }
        return (double)total / (double)data.size();
    }
}

class Adder extends Thread {

    private static int count = 0;

    int id = count;
    int lo, hi;
    int[] x;

    Adder(int[] xx, int l, int h) {
        count++;
        x = xx;
        lo = l;
        hi = h;
    }

    public void run() {
        for (int i = lo; i <= hi; i++) {
            synchronized (ArraySum.lock) {
                ArraySum.sum = ArraySum.sum + x[i];
            }
            yield();
        }
    }

}

class AdderLocal extends Thread {

    private static int count = 0;

    int id = count;
    int lo, hi;
    int[] x;

    public int sum;

    AdderLocal(int[] xx, int l, int h) {
        count++;
        x = xx;
        lo = l;
        hi = h;
    }

    public void run() {
        for (int i = lo; i <= hi; i++) {
            sum +=  x[i];
            yield();
        }
    }
}

class AdderRec extends Thread {

    private static int count = 0;
    public static int Threashold = 1000;

    int id = count;
    int lo, hi;
    int[] x;

    public int sum;

    AdderRec(int[] xx, int l, int h) {
        count++;
        x = xx;
        lo = l;
        hi = h;
    }

    public void run() {
        int size = hi - lo;
        if (size > Threashold) {
            // split
            int nextLo = lo + (size/2);
            AdderRec lower = new AdderRec(x, lo, nextLo - 1);
            AdderRec higher = new AdderRec(x, nextLo, hi);

            lower.start();
            higher.start();

            try {
                lower.join();
                higher.join();
            } catch (InterruptedException e) {
                System.err.println("Ouch");
            }

            sum = lower.sum + higher.sum;
        } else {
            for (int i = lo; i <= hi; i++) {
                sum +=  x[i];
                yield();
            }
        }
    }
}
