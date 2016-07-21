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

    private static void Sum0() {

        System.out.print("\nNo threads");
        StopWatch w = new StopWatch();
        w.start();

        int sum = 0;
        for (int i = 0; i < M; i++) sum = sum + x[i];

        w.stop();
        System.out.print("   Sum = " + sum);
        System.out.println("  elapsed time: " + w.getElapsedTime() + " ms");

    }

    // Sum with threads

    private static void SumN(int N) {

        System.out.print("\n" + N + " threads");
        StopWatch w = new StopWatch();
        w.start();

        sum = 0;
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

        // Wait till they're all finished, then print a message

        try {
            for (int k = 0; k < N; k++) {
                adder[k].join();
            }
        } catch (InterruptedException e) {
            System.out.println("Ouch");
        }

        w.stop();
        System.out.print("   Sum = " + sum);
        System.out.println("  elapsed time: " + w.getElapsedTime() + " ms");

    }


    public static void main(String[] args) {

        // Check for arguments

        if (args.length != 0 && args.length != 2) {
            System.out.println("Error: Must provide two arguments or none!");
            System.exit(1);
        }

        if (args.length == 2) {
            N = Integer.parseInt(args[0]);
            M = Integer.parseInt(args[1]);
        }

        // Create and initialise array to 0..M-1.

        x = new int[M];
        for (int i = 0; i < M; i++) x[i] = i + 1;

        System.out.println("ArraySum with " + N + " threads, " + M + " elements");
        System.out.println("Sum is " + (M * (M + 1) / 2));

        Sum0();

        SumN(N);

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
