// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

import java.util.*;

/**
 * Implements a priority queue based on a heap that is represented with an
 * array.
 */
public class HeapArrayQueue<E extends Comparable<? super E>> extends
        AbstractQueue<E> {

    @SuppressWarnings("unchecked")
    private E[] data = (E[]) (new Comparable[7]);
    private int count = 0;

    public int size() {
        return count;
    }

    public boolean isEmpty() {
        return size() == 0;
    }

    /**
     * Returns the element with the top priority in the queue.
     * <p/>
     * HINT: This is like 'poll()' without the removal of the element.
     *
     * @returns the next element if present, or 'null' if the queue is empty.
     */
    public E peek() {
        return data[0];
    }

    /**
     * Removes the element with the top priority from the queue and returns it.
     * <p/>
     * HINT: The 'data' array should contain a heap so the element with the top
     * priority sits at index '0'. After its removal, you need to restore the
     * heap property again, using 'sinkDownFromIndex(...)'.
     *
     * @returns the next element in the queue, or 'null' if the queue is empty.
     */
    public E poll() {
        E returned = data[0];
        count--;
        swap(0, count);
        sinkDownFromIndex(0);
        return returned;
    }

    /**
     * Enqueues an element.
     * <p/>
     * If the element to be added is 'null', it is not added.
     * <p/>
     * HINT: Make use of 'ensureCapacity' to make sure that the array can
     * accommodate one more element.
     *
     * @param element - the element to be added to the queue
     * @returns true, if the element could be added
     */
    public boolean offer(E element) {
        ensureCapacity();

        data[count] = element;
        bubbleUpFromIndex(count);
        count++;
        return true;
    }

    private void sinkDownFromIndex(int nodeIndex) {
        if (nodeIndex >= count)
            return;

        E current = data[nodeIndex];
        E left = (nodeIndex * 2 < count ? data[nodeIndex * 2] : null);
        E right = (nodeIndex * 2 + 1 < count ? data[nodeIndex * 2 + 1] : null);


        if (right == null && left == null) {
        } else if (right == null && current.compareTo(left) < 0) {
            swap(nodeIndex, nodeIndex * 2);
            sinkDownFromIndex(nodeIndex * 2);
        } else if (left == null && current.compareTo(right) < 0) {
            swap(nodeIndex, nodeIndex * 2 + 1);
            sinkDownFromIndex(nodeIndex * 2 + 1);
        } else if (right != null && left != null) {
            boolean leftCheck = current.compareTo(left) < 0;
            boolean rightCheck = current.compareTo(right) < 0;

            if (leftCheck && rightCheck) {
                if (current.compareTo(left) < current.compareTo(right)) {
                    swap(nodeIndex, nodeIndex * 2);
                    sinkDownFromIndex(nodeIndex * 2);
                } else {
                    swap(nodeIndex, nodeIndex * 2 + 1);
                    sinkDownFromIndex(nodeIndex * 2 + 1);
                }
            } else if (leftCheck) {
                swap(nodeIndex, nodeIndex * 2);
                sinkDownFromIndex(nodeIndex * 2);
            } else if (rightCheck) {
                swap(nodeIndex, nodeIndex * 2 + 1);
                sinkDownFromIndex(nodeIndex * 2 + 1);
            }
        }
    }

    private void bubbleUpFromIndex(int nodeIndex) {
        if (data[nodeIndex].compareTo(data[nodeIndex / 2]) < 0) {
            swap(nodeIndex, nodeIndex / 2);
            bubbleUpFromIndex(nodeIndex / 2);
        }
    }

    /**
     * Swaps two elements in the supporting array.
     */
    private void swap(int from, int to) {
        E temp = data[from];
        data[from] = data[to];
        data[to] = temp;
    }

    /**
     * Increases the size of the supporting array, if necessary
     */
    private void ensureCapacity() {
        if (count == data.length) {
            @SuppressWarnings("unchecked")
            E[] newData = (E[]) new Comparable[data.length * 2];

            // copy data elements
            for (int loop = 0; loop < count; loop++) {
                newData[loop] = data[loop];
            }
            data = newData;
        }
    }

    // no iterator implementation required for this assignment
    public Iterator<E> iterator() {
        return null;
    }
}
