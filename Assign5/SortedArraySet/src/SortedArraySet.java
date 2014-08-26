// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 103, Assignment 5
 * Name:
 * Usercode:
 * ID:
 */

import java.util.*;

/**
 * SortedArraySet - a Set collection;
 *
 * The implementation uses an array and a count to store the items. The items in
 * the set should be stored in positions 0, 1,... (count-1) of the array The
 * size of the array when the set is first created should be 10. It keeps the
 * items in order according to the comparator. Ie, when it adds a new item, it
 * must put it in the right place When it searches for an item, it should use
 * binary search. Note, the comparator assumes that the items are Comparable. It
 * does not allow null items or duplicates. Attempting to add null should throw
 * an exception Adding an item which is already present should simply return
 * false, without changing the set. It should always compare items using
 * equals() (not using ==) When full, it will create a new array of double the
 * current size, and copy all the items over to the new array
 */

public class SortedArraySet<E> extends AbstractSet<E> {

	// Data fields
	private static int INITIALCAPACITY = 10;
	private E[] data;
	private int count = 0;

	private Comparator<E> comp; // use comp to compare items.

	// --- Constructors --------------------------------------
	/** Constructor to make a new empty set */
	@SuppressWarnings("unchecked")
	// this will stop Java complaining about the cast
	public SortedArraySet() {
		comp = new ComparableComparator();
		data = (E[]) new Object[INITIALCAPACITY];
	}

	/** Constructor to make a new empty set, with a given comparator */
	@SuppressWarnings("unchecked")
	// this will stop Java complaining about the cast
	public SortedArraySet(Comparator<E> comparator) {
		comp = comparator;
		data = (E[]) new Object[INITIALCAPACITY];
	}

	/** Constructor that takes a whole collection and sorts it all at once */
	@SuppressWarnings("unchecked")
	// this will stop Java complaining about the cast
	public SortedArraySet(Collection<E> col) {
		comp = new ComparableComparator();

		if (col instanceof List<?>) {
			Collections.sort((List<E>) col, comp);
			data = (E[]) col.toArray();
			count = col.size();
		} else {
			for (E i : col)
				add(i);
		}
	}

	/** Constructor that takes a whole collection and sorts it all at once */
	@SuppressWarnings("unchecked")
	// this will stop Java complaining about the cast
	public SortedArraySet(Collection<E> col, Comparator<E> comparator) {
		comp = comparator;

		if (col instanceof List<?>) {
			Collections.sort((List<E>) col, comp);
			data = (E[]) col.toArray();
			count = col.size();
		} else {
			for (E i : col)
				add(i);
		}
	}

	// --- Methods --------------------------------------

	/**
	 * Returns number of items in collection as integer
	 */
	public int size() {
		return count;
	}

	/**
	 * Add the specified item to this set (if it is not a duplicate of an item
	 * already in the set). Will not add the null value (throws an
	 * IllegalArgumentException in this case) Return true if the collection
	 * changes, and false if it did not change.
	 */
	public boolean add(E item) {
		if (item == null) {
			throw new IllegalArgumentException();
		}

		if (contains(item))
			return false;

		ensureCapacity();

		// Insertion Sort
		data[count] = item;
		for (int i = count - 1; i >= 0; i--) {
			if (comp.compare(data[i], data[i + 1]) > 0) {
				E tmp = data[i];
				data[i] = data[i + 1];
				data[i + 1] = tmp;
			} else {
				break;
			}
		}

		count++;
//		printArray();
//		System.out.println("Count now : " + count);
		return true;
	}

	/** Return true if this set contains the specified item. */
	@SuppressWarnings("unchecked")
	// stops Java complaining about the call to compare
	public boolean contains(Object item) {
		if (item == null)
			return false;
		E itm = (E) item;
		return (findIndex(itm) != -1);
	}

	/**
	 * Remove an item matching a given item Return true if the item was present
	 * and then removed. Make no change to the set and return false if the item
	 * is not present.
	 */
	@SuppressWarnings("unchecked")
	// stops Java complaining about the call to compare
	public boolean remove(Object item) {
		if (item == null)
			return false;

		int index = findIndex((E) item);
		if (index == -1)
			return false;

		data[index] = null;
		// Move down the bus
		for (int i = index + 1; i < count; i++) {
			data[i - 1] = data[i];
		}
		count--;
		return true;
	}

	// It is much more convenient to define the following method
	// and use it in the methods above.

	/**
	 * Find the index of where an item is in the dataarray, (or where it ought
	 * to be, if it's not there). Assumes that the item is not null. Uses binary
	 * search and requires that the items are kept in order. Should use
	 * compareTo to compare values
	 */
	private int findIndex(E item) {
//		if (item instanceof String)
//			return fintIndexString(item);

		if (item == null)
			return -1;

		if (count == 0)
			return -1;

//		System.out.println("Looking for : " + item);
		int low = 0, mid = count / 2, high = count;
		while (low < high) {
//			System.out.println("Low : " + low + " Mid : " + mid + " High : "
//					+ high);

			if (comp.compare(data[mid], item) == 0) {
				System.out.println("Found at : " + mid);
				return mid;
			}

			int nmid = (high + low) / 2;
			if (comp.compare(data[mid], item) < 0) {
				low = mid + 1;
				mid = nmid;
			} else {
				high = mid;
				mid = nmid;
			}
		}
		return -1;
	}

//	private int fintIndexString(E item) {
//
//		if (!(item instanceof String))
//			return findIndex(item);
//
//		if (count == 0)
//			return -1;
//
//		int low = 0, mid = count / 2, high = count, index = 0;
//		String itm = (String)item;
//		System.out.println("Finding " + item);
//		while (low < high) {
//			System.out.println("Low : " + low + " Mid : " + mid + " High : " + high + 
//					"\nLow : " + data[low] + " Mid : " + data[mid] + " High : " + data[high - 1]);
//			
//			if (comp.compare(data[mid], item) == 0) {
//				System.out.println("Found at : " + mid);
//				return mid;
//			}
//
//			String ls = (String) (data[low]);
//			if (ls.equals(item))
//				return low;
//			String hs = (String) (data[high - 1]);
//			if (hs.equals(item))
//				return high - 1;
//			
//			double delta = -1;
//			double itemc = 0;
//			do {
//				delta = (double)(hs.charAt(index) - ls.charAt(index));
//				if (delta == 0) {
//					index++;
//				}
//			} while (delta == 0 && index < hs.length() && index < ls.length() && index < itm.length());
//			
//			if (delta == 0.0)
//				break;
//			else
//				delta++;
//			
//			
//			if (index >= hs.length() || index >= ls.length() || index >= itm.length()) {
//				if (itemc >= delta) {
//					System.out.println("Break by def");
//					break;
//				}
//				index--;
//			}
//
//			int nmid;
//			boolean gtr = false;
//			if (comp.compare(data[mid], item) <= 0) {
//				low = mid + 1;
//				
//				itemc = (double)(itm.charAt(index) - ls.charAt(index));
//				if (itemc != 0)
//					nmid = (int)((itemc/delta)*((high + low) / 2)) + low + 1;
//				else
//					nmid = (high + low) / 2;
//				
//				nmid = Math.min( Math.max(low, nmid), high -1);
//				mid = nmid;
//				gtr = false;
//			} else {
//				high = mid;
//				itemc = (double)(hs.charAt(index) - itm.charAt(index));
//				if (itemc != 0)
//					nmid = (int)((itemc/delta)*((high + low) / 2)) + low + 1;
//				else
//					nmid = (high + low) / 2;
//				
//				nmid = Math.min( Math.max(low, nmid), high -1);
//				mid = nmid;
//				gtr = true;
//			}
//			
//			System.out.println("Item : " + itemc + " / Delta : " + delta + " Calc: " + nmid + " Gtr : " + (gtr ? "H" : "L"));
//			
//			System.out.println("");
//		}
//		return -1;
//	}

	private void printArray() {
		System.out.print("[");
		for (int i = 0; i < count; i++)
			System.out.print(" " + data[i] + (i != count - 1 ? "," : " "));
		System.out.println("]");

	}

	/**
	 * Ensure data array has sufficient number of items to add a new item
	 */
	@SuppressWarnings("unchecked")
	// this will stop Java complaining about the cast
	private void ensureCapacity() {
		if (count < data.length)
			return;
		E[] newArray = (E[]) (new Object[data.length * 2]);
		for (int i = 0; i < count; i++)
			newArray[i] = data[i];
		data = newArray;
	}

	// --- Iterator and Comparator --------------------------------------

	/** Return an iterator over the items in this set. */
	public Iterator<E> iterator() {
		return new SortedArraySetIterator(this);
	}

	private class SortedArraySetIterator implements Iterator<E> {
		// needs fields, constructor, hasNext(), next(), and remove()
		private SortedArraySet<E> set;
		private int nextIndex = 0;
		private boolean canRemove = false;

		private SortedArraySetIterator(SortedArraySet<E> s) {
			set = s;
		}

		/** Return true if iterator has at least one more item */
		public boolean hasNext() {
			return (nextIndex < set.count);
		}

		/** Return next item in the set */
		public E next() {
			if (nextIndex >= set.count)
				throw new NoSuchElementException();
			canRemove = true;
			return set.data[nextIndex++];
		}

		/**
		 * Remove from the set the last item returned by the iterator. Can only
		 * be called once per call to next.
		 */
		public void remove() {
			if (!canRemove)
				throw new IllegalStateException();
			set.remove(set.data[nextIndex - 1]);
			canRemove = false;
		}
	}

	/**
	 * This is a comparator that assumes that E's are Comparable: it casts them
	 * to Comparable<E>, and then calls their compare method. It will fail if
	 * E's are not Comparable - in this case, the set should have been
	 * constructed with an appropriate comparator.
	 */
	private class ComparableComparator implements Comparator<E> {
		@SuppressWarnings("unchecked")
		// this will stop Java complaining about the cast
		public int compare(E item, E other) {
			if (item == null && other == null)
				throw new IllegalArgumentException("Cannot Compare NULL items");

			if (item == null)
				return 1;

			if (other == null)
				return -1;

			Comparable<E> itm = (Comparable<E>) item;
			return itm.compareTo(other);
		}
	}
}
