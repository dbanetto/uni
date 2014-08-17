// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


import java.util.*;
import java.util.concurrent.*;

/**
 * This class is for the challenge.
 * It includes two private classes - Producer and Consumer - which extend Thread.
 * That means that calling start() on a Producer or Consumer object
 * will start a new thread and call their run() method.
 * The Producer objects put values on the queue
 * The Consumer objects take values off the queue, and "process"
 *  them by removing the values from the remainingValues Set
 * The program runs until all the values have been "processed".
 * 
 */
public class ThreadChallenge {

    public static final int NUM_PRODUCERS = 20;
    public static final int NUM_CONSUMERS = 20;
    public static final int NUM_PER_THREAD = 10000;

    private final BlockingQueue<String> queue;

    public ThreadChallenge() {
        // Replace with your ArrayQueue
        queue = new java.util.concurrent.ArrayBlockingQueue<String>(NUM_PER_THREAD);
        //queue = new ArrayQueue<String>();
        //queue = new ArrayQueueCh<String>();
    }

    public void run() throws InterruptedException {
        Set<String> remainingValues = Collections.synchronizedSet(new HashSet<String>());
        List<Producer> producers = new ArrayList<Producer>();
        List<Consumer> consumers = new ArrayList<Consumer>();

        // create the remainingValues and the producers
        int count = 0;
        for (int i = 0; i < NUM_PRODUCERS; i++) {
            List<String> input = new ArrayList<String>();
            for (int j = 0; j < NUM_PER_THREAD; j++) {
                String item = "item" + count;
                count++;
                remainingValues.add(item);
                input.add(item);
            }
            producers.add(new Producer(i + 1, queue, input));
        }

        // create the consumers
        for (int i = 0; i < NUM_CONSUMERS; i++) {
            consumers.add(new Consumer(i + 1, queue, remainingValues));
        }

        // start the consumers
        for (Consumer consumer : consumers) {
            consumer.start();
        }

        // start the producers
        for (Producer producer : producers) {
            producer.start();
        }

        // wait for the consumers to finish removing items from the set
        while (!remainingValues.isEmpty()) {
            Thread.sleep(10);
        }

        // print out the results
        System.out.println("Done:");
        for (Consumer consumer : consumers) {
            System.out.println(consumer);
        }

        // terminate nicely by telling the consumer threads that there are
        // no more elements in the queue 
        for (Consumer c : consumers) {
            c.interrupt();
        }
    }

    private class Producer extends Thread {
        private final BlockingQueue<String> queue;
        private final Iterable<String> input;

        public Producer(int i, BlockingQueue<String> queue, Iterable<String> input) {
            super("Producer " + i);
            this.queue = queue;
            this.input = input;
        }

        public void run() {
            try {
                for (String item : input) {
                    queue.put(item);
                }
            }
            catch (InterruptedException ex) {}
        }
    }

    private class Consumer extends Thread {
        private final BlockingQueue<String> queue;
        private final Set<String> data;
        private final String name;
        private int consumed = 0;

        public Consumer(int i, BlockingQueue<String> queue, Set<String> data) {
            super("Consumer " + i);
            this.name = "Consumer " + i;
            this.queue = queue;
            this.data = data;
        }

        /**
         * Removes items from the queue using take().
         * Take should 'block' until an item is available.
         * Each item returned from the queue is removed from the set.
         */
        public void run() {
            try {
                while (true) {
                    String item = queue.take();
                    consumed++;
                    boolean removed = data.remove(item);
                    if (!removed) {
                        throw new RuntimeException(item + " was not in the table!");
                    }
                }
            }
            catch (InterruptedException ex) {}
        }

        public String toString() {
            return "\t" + name + ": " + consumed;
        }
    }

    public static void main(String[] args) throws InterruptedException {
        new ThreadChallenge().run();
    }
}
