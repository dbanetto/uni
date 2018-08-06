// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

import java.util.*;
import java.io.*;
import ecs100.*;

/**
 * Implementation of a Set type using a Binary Search Tree to store the items.
 * 
 * The Set does not allow null elements and obviously no duplicates.
 * 
 * Attempting to add null should throw an exception. An attempt at adding an
 * element which is already present should simply return false, without changing
 * the set.
 * 
 * @author: Thomas Kuehne (based on previous code)
 */
public class BSTSet<E extends Comparable<E>> extends AbstractSet<E> {

	// the root of the supporting binary search tree
	private BSTNode<E> root;

	// number of elements in the set
	private int count = 0;

	/**
	 * Returns true if the set is empty
	 */
	public boolean isEmpty() {
		return count == 0;
	}

	/**
	 * Returns the number of elements in set
	 */
	public int size() {
		return count;
	}

	/**
	 * Returns true if the set contains 'item'
	 * 
	 * CORE
	 *
	 * HINT: There is no need to search for 'null' arguments.
	 * 
	 * HINT: After checking special cases, delegate the work to the root node of
	 * the supporting tree.
	 *
	 * @param item
	 *            - the item to check for
	 *
	 */
	public boolean contains(E item) throws ClassCastException {
		if (item == null)
			return false;
		
		return (root != null ? root.contains(item) : false);
	}

	/**
	 * Adds the specified element to the set, as long as it is not null or is
	 * not already in the set.
	 * 
	 * CORE
	 * 
	 * HINT: Remember to update the count field as well.
	 * 
	 * HINT: You can check whether the item exists already in an extra step or
	 * perform the checking while attempting to add the item. The latter is more
	 * efficient.
	 * 
	 * @param item
	 *            - the item to be added
	 * 
	 * @returns true if the collection changes, and false if it does not change.
	 * 
	 */
	public boolean add(E item) {
		if (item == null)
			throw new IllegalArgumentException(
					"Null cannot be added to a BSTSet");

		if (this.root != null) {
			if (this.root.add(item)) {
				count++;
				return true;
			} else {
				return false;
			}
		} else {
			root = new BSTNode<E>(item);
			count = 1;
			return true;
		}
	}

	/**
	 * Returns the height of the tree.
	 * 
	 * CORE
	 * 
	 * HINT: You probably want to make use of method 'height()' in class
	 * BSTNode.
	 * 
	 * @returns the height of the tree -1 if the tree is empty, 0 if there is
	 *          just a root node, and greater than 0 in all other cases.
	 * 
	 */
	public int height() {
		/* # YOUR CODE HERE */
		return (root != null ? root.height() : -1);
	}

	/**
	 * 
	 * Returns the minimum depth of the tree.
	 * 
	 * COMPLETION
	 * 
	 * HINT: You probably want to make use of method 'minDepth()' in class
	 * BSTNode.
	 * 
	 * @returns the length of the shortest branch in the tree -1 if the tree is
	 *          empty, 0 if there is just a root node, and greater than 0 in all
	 *          other cases.
	 * 
	 */
	public int minDepth() {
		/* # YOUR CODE HERE */
		return (root != null ? root.minDepth() : -1);
	}

	/**
	 * Removes the element matching a given item.
	 * 
	 * COMPLETION
	 * 
	 * Note that any children of the removed node must be kept in the tree.
	 * 
	 * HINT: Remember to update the count field, if necessary. HINT: A null item
	 * need not be removed. HINT: You may use 'remove(...)' from class BSTNode
	 * as a helper, but you do not have to.
	 * 
	 * @param item
	 *            - the item to remove
	 * @returns true if the collection changes, and false if it did not change.
	 */
	public boolean remove(E item) {
		if (root != null) {
			if (!root.contains(item))
				return false;
			root = root.remove(item);
			count--;
			return true;
		}
		return false;
	}

	/**
	 * @returns an iterator over the elements in this set.
	 */
	public Iterator<E> iterator() {
		// start a new iteration on the root node
		return new BSTSetIterator(root);
	}

	/**
	 * Iterator for BST.
	 * 
	 * CHALLENGE
	 * 
	 * Iterates through the tree in sorted order, i.e. using in-order traversal.
	 * Uses a stack to keep track of the current position (iteration state).
	 * 
	 * HINTS: To accomplish this, the iterator must process the complete left
	 * subtree of a node before it processes the node. Therefore, every time it
	 * pushes a node on the stack, it can also immediately push all the left
	 * descendents of the node on to the stack, all the way to the leftmost
	 * descendent.
	 * 
	 * When it pops a node from the stack, it processes the node and then pushes
	 * its right child onto the stack (and all that child's left descendents).
	 * 
	 * There is a dedicated video on Blackboard --
	 * "Comp103, Iterative In-Order Traversal, Variant 2" -- explaining this
	 * particular approach.
	 * 
	 * You may choose another approach as well.
	 */
	private class BSTSetIterator implements Iterator<E> {

		Stack<BSTNode<E>> nodeStack = new Stack<BSTNode<E>>();
		BSTNode<E> pointer = null;
		/**
		 * Creates an iterator.
		 * 
		 * CHALLENGE
		 * 
		 * HINT: Populate the 'to do' stack as much as necessary already.
		 */
		public BSTSetIterator(BSTNode<E> root) {
			pointer = root;
		}

		/**
		 * Returns whether there is still something to iterate
		 * 
		 * CHALLENGE
		 * 
		 */
		public boolean hasNext() {
			return (!nodeStack.isEmpty() || pointer != null);
		}

		/**
		 * Returns the next element of the current iteration.
		 * 
		 * CHALLENGE
		 * 
		 * HINT: Remove an element from the 'to do' stack and return it, but
		 * before returning, make sure you add new elements to the stack, if
		 * appropriate.
		 * 
		 */
		public E next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			} else {
				//Go to the bottom left
				while (pointer != null) {
					nodeStack.push(pointer); //Add as you go
					pointer = pointer.getLeft(); // Go to the left
				}
				// Reach bottom, pointer is null
				
				// Set Pointer to last seen (bottom left) Is "centre" for this node
				pointer = nodeStack.pop();
				// Get output value
				BSTNode<E> node = pointer;
				// Go right, as there is no left
				pointer = pointer.right;
				
				return (E)(node.getValue());
			}
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

	/**
	 * Prints all the elements to a stream.
	 * 
	 * @param stream
	 *            - the output stream
	 */
	public void printAllToStream(PrintStream stream) {
		// if there is nothing to do...
		if (isEmpty())
			return;

		// traverse tree, passing on the output stream
		root.printAllToStream(stream);
	}

	public static void main(String[] args) {
		testing();
	}

	public static void testing() {
		BSTSet<String> testBST = new BSTSet<String>();

		String[] testItems = new String[] { "N", "G", "T", "C", "W", "O", "K",
				"U", "V", "E", "Y", "Z", "S", "A", "M", "P", "R", "I", "L",
				"F", "J", "B", "D", "H", "X", "Q" };

		for (String item : testItems) {
			UI.println("Adding " + item);
			testBST.add(item);
			UI.println("----------------------------");

			if (testBST.isEmpty())
				UI.println("Empty Tree");
			else
				testBST.printAll();

			UI.println("Height: " + testBST.height());
			UI.println("Min Depth: " + testBST.minDepth());
			UI.println("----------------------------");
		}
	}

	public void printAll() {
		root.printAll("");
	}
}
