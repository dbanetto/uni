// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP 103, Assignment 4
 * Name:
 * Usercode:
 * ID:
 */

import java.util.*;

/**
 * ArraySet - a Set collection;
 *
 * The implementation uses an array and a count to store the items.
 *  The items in the set should be stored in positions
 *  0, 1,... (count-1) of the array
 * The size of the array when the set is first created should be 10. 
 * It does not keep the items in any particular order, and may change the
 *  order of the remaining items when removing items.  Eg, it can always add
 *  a new item at the end, and it can move the last item into the place of an
 *  item being deleted - there is no need to shift all  the items up or down
 *  to keep them in order.
 * It does not allow null items or duplicates.
 *  Attempting to add null should throw an exception
 *  Adding an item which is already present should simply return false, without
 *  changing the set.
 * It should always compare items using equals()  (not using ==)
 * When full, it will create a new array of double the current size, and
 *  copy all the items over to the new array
 */

public class ArraySet <E> extends AbstractSet <E> {

    // Data fields
    /*# YOUR CODE HERE */

    // Constructors

    @SuppressWarnings("unchecked")  // this will stop Java complaining
    public ArraySet() {
        /*# YOUR CODE HERE */
    }

    // Methods

    /** Returns number of items in collection as integer 
     */
    public int size () {
        /*# YOUR CODE HERE */
    }

    /** Add the specified item to this set (if it is not a duplicate of an item
     *  already in the set).
     *  Will not add the null value (throws an IllegalArgumentException in this case)
     *  Return true if the collection changes, and false if it did not change.
     */
    public boolean add(E item) {
        /*# YOUR CODE HERE */
    }

    /** Return true if this set contains the specified item. */
    public boolean contains(Object item) {
        /*# YOUR CODE HERE */
    }

    /** Remove an item matching a given item
     *  Return true if the item was present and then removed.
     *  Make no change to the set and return false if the item is not present.
     */
    public boolean remove (Object item) {
        /*# YOUR CODE HERE */
    }

    /** Return an iterator over the items in this set. */
    public Iterator <E> iterator() {
        /*# YOUR CODE HERE */
    }

    /** Ensure data array has sufficient number of items
     *  to add a new item 
     */
    @SuppressWarnings("unchecked")  // this will stop Java complaining
    private void ensureCapacity () {
        /*# YOUR CODE HERE */
    }

    // You may find it convenient to define the following method and use it in
    // the methods above, but you don't need to do it this way.

    /** Find the index of an item in the dataarray, or -1 if not present
     *  Assumes that the item is not null 
     */
    private int findIndex(Object item) {
        /*# YOUR CODE HERE */
    }

    private class ArraySetIterator <E> implements Iterator <E> {

        // needs fields, constructor, hasNext(), next(), and remove()

        /*# YOUR CODE HERE */

        /** Return true if iterator has at least one more item */
        public boolean hasNext() {
            /*# YOUR CODE HERE */
        }

        /** Return next item in the set */
        public E next() {
            /*# YOUR CODE HERE */
        }

        /** Remove from the set the last item returned by the iterator.
         *  Can only be called once per call to next.
         */
        public void remove() {
            /*# YOUR CODE HERE */
        }
    }
}

