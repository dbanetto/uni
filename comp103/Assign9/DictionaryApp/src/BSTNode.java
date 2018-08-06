// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

import java.io.*;
import ecs100.*;

/**
 * Implements a binary search tree node.
 * 
 * @author: Thomas Kuehne (based on previous code)
 */

public class BSTNode<E extends Comparable<E>> {

	private E value;
	private BSTNode<E> left;
	public BSTNode<E> right;

	// constructs a node with a value
	public BSTNode(E value) {
		this.value = value;
	}

	// Getters...

	public E getValue() {
		return value;
	}

	public BSTNode<E> getLeft() {
		return left;
	}

	public BSTNode<E> getRight() {
		return right;
	}

	/**
	 * Returns true if the subtree formed by the receiver contains 'item'
	 * 
	 * CORE
	 *
	 * ASSUMPTION: 'item' is not 'null'.
	 * 
	 * HINT: A recursive approach leads to a very short and simple code.
	 * 
	 * HINT: use 'compareTo(...)' in order to compare the parameter with the
	 * data in the node.
	 * 
	 * HINT: Make sure that you invoke 'compareTo' by always using the same
	 * receiver / argument ordering throughout the program, e.g., always use the
	 * item as the receiver of 'compareTo'.
	 *
	 * @param item
	 *            - the item to check for
	 * @returns true if the subtree contained 'item'
	 *
	 */
	public boolean contains(E item) {
		return (find(item) != null);
	}
	/**
	 * Iterative find to prevent stack overflows
	 */
	public BSTNode<E> find (E item) {
		BSTNode<E> pointer = this;
		while (pointer != null) {
			if (pointer.value.compareTo(item) == 0) {
				return pointer;
			}
			if (pointer.value.compareTo(item) > 0) {
				pointer =  pointer.left;
			}else if (pointer.value.compareTo(item) < 0) {
				pointer =  pointer.right;
			}
		}
		return null;
	}
	
	/**
	 * Adds an item to the subtree formed by the receiver.
	 * 
	 * CORE
	 * 
	 * Must not add an item, if it is already in the tree.
	 * 
	 * HINT: use 'compareTo(...)' in order to compare the parameter with the
	 * data in the node.
	 * 
	 * @param item
	 *            - the value to be added
	 * @returns false, if the item was in the subtree already. Returns true
	 *          otherwise.
	 */
	public boolean add(E item) {
		if (value.compareTo(item) > 0) {
			if (this.left == null) {
				left = new BSTNode<E>(item);
				return true;
			} else {
				return left.add(item);
			}
		} else if ((value.compareTo(item) < 0)) {
			if (right == null) {
				right = new BSTNode<E>(item);
				return true;
			} else {
				return right.add(item);
			}
		} else {
			return false;
		}
	}

	/**
	 * Returns the height of the receiver node.
	 * 
	 * CORE
	 * 
	 * HINT: The number of children the receiver node may have, implies four
	 * cases to deal with (none, left, right, left & right).
	 * 
	 * @returns the height of the receiver
	 */
	public int height() {
		int height = 0;
		if (left != null)
			height = Math.max(height, left.height() + 1);
		if (right != null)
			height = Math.max(height, right.height() + 1);

		return height;
	}

	/**
	 * Returns the length of the shortest branch in the subtree formed by the
	 * receiver.
	 * 
	 * COMPLETION
	 * 
	 * @returns the minimum of all branch lenghts starting from the receiver.
	 * 
	 */
	public int minDepth() {
		int height = Integer.MAX_VALUE;
		if (left != null)
			height = Math.min(height, left.height() + 1);
		if (right != null)
			height = Math.min(height, right.height() + 1);
		if (left == null && right == null)
			height = 0;
		return height;
	}

	/**
	 * Removes an item in the subtree formed by the receiver.
	 * 
	 * COMPLETION
	 * 
	 * ASSUMPTION: The item to be removed does exist. The case that it cannot be
	 * found, should be dealt with before this method is called.
	 * 
	 * Performs two tasks: 1. locates the node to be removed, and 2. replaces
	 * the node with a suitable node from its subtrees.
	 * 
	 * HINT: use 'compareTo(...)' in order to compare the parameter with the
	 * data in the node.
	 * 
	 * HINT: For task 2, you should use call method
	 * 'replacementSubtreeFromChildren' to obtain this node.
	 * 
	 * HINT: When replacing a node, it is sufficient to change the value of the
	 * existing node with the value of the node that conceptually replaces it.
	 * There is no need to actually replace the node object as such.
	 * 
	 * @param item
	 *            - the item to be removed
	 * @returns the reference to the subtree with the item removed.
	 * 
	 *          HINT: Often the returned reference will be the receiver node,
	 *          but it is possible that the receiver itself needs to be removed.
	 *          If you use a recursive approach, the latter case is the base
	 *          case.
	 * 
	 */
	public BSTNode<E> remove(E item) {
		if (value.compareTo(item) > 0 && left != null) {
			left = left.remove(item); // Find node to delete
		} else if (value.compareTo(item) < 0 && right != null) {
			right = right.remove(item); // Find node to delete
		} if (value.compareTo(item) != 0) {
			return this;
		}
		
		if (left != null && right != null) {
			return replacementSubtreeFromChildren(left,right);
		} else if (left != null && right == null) {
			return replacementSubtreeFromChildren(left,right);
		} else if (right != null && left == null) {
			return replacementSubtreeFromChildren(left,right);
		}
		return null;
	}

	/**
	 * Returns a replacement subtree for the receiver node (which is to be
	 * removed).
	 * 
	 * COMPLETION
	 * 
	 * The replacement subtree is determined from the children of the node to be
	 * removed.
	 * 
	 * HINT: There are several cases: - node has no children => return null -
	 * node has only one child => return the child - node has two children =>
	 * return the current subtree but with a) its (local) root replaced by the
	 * leftmost node in the right subtree, and b) the leftmmost node in the
	 * right subtree removed.
	 * 
	 * @param left
	 *            - the left subtree from which to include items.
	 * @param right
	 *            - the right subtree from which to include items.
	 * @returns a reference to a subtree which contains all items from 'left'
	 *          and 'right' combined.
	 * 
	 */
	private BSTNode<E> replacementSubtreeFromChildren(BSTNode<E> left,
			BSTNode<E> right) {
		
		if (left == null && right == null)
			return null;
		
		if (left != null && right == null)
			return left;
		if (left == null && right != null)
			return right;
		
		BSTNode<E> root = null;
		
		root = right;
		while (root.left != null) {
			root = root.left;
		}
		right = right.remove(root.value);
		
		
		root.left = left;
		root.right = right;
		
		// not a simple case => return modified node
		return root;
	}

	/**
	 * Returns the leftmost node in the subtree formed by the receiver.
	 * 
	 * COMPLETION
	 * 
	 * HINT: The code is very simple. Just keep descending left branches, until
	 * it is no longer possible.
	 * 
	 * @returns a reference to the leftmost node, starting from the receiver.
	 * 
	 */
	private BSTNode<E> getLeftmostNode() {
		if (this.left != null) {
			return this.left.getLeftmostNode();
		} else {
			return this;
		}
	}

	/**
	 * Prints all the nodes in a subtree to a stream.
	 * 
	 * @param stream
	 *            - the output stream
	 */
	public void printAllToStream(PrintStream stream) {
		if (left != null)
			left.printAllToStream(stream);

		stream.println(value);

		if (right != null)
			right.printAllToStream(stream);
	}

	/**
	 * Prints all the nodes in a subtree on the text pane.
	 * 
	 * Can be useful for debugging purposes, but is most useful on small sample
	 * trees.
	 * 
	 * Usage: node.printAll("").
	 */
	public void printAll(String indent) {
		if (right != null)
			right.printAll(indent + "    ");

		UI.println(indent + value);

		if (left != null)
			left.printAll(indent + "    ");
	}
}
