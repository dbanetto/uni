// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/**
 * Class ImageNode implements a node that forms a linked list data structure in
 * conjunction with other nodes of the same type.
 * 
 * A node represents an images by storing the filename of the image. A node
 * furthermore links to their successor node.
 * 
 * @author Thomas Kuehne
 * @version 5 September 2013
 */
public class ImageNode {

	// A string that contains the full file name of an image file.
	private String imageFileName;

	// A reference to the next ImageNode in the linked list.
	private ImageNode next;

	/**
	 * Creates an image node
	 * 
	 * @param imageFileNameStr
	 *            the file name of the image to be represented by this node
	 * @param nextNode
	 *            the reference to the next node in the list
	 */
	public ImageNode(String imageFileNameStr, ImageNode nextNode) {
		this.imageFileName = imageFileNameStr;
		this.next = nextNode;
	}

	/**
	 * Returns the filename of the image.
	 */
	public String getFileName() {
		return imageFileName;
	}

	/**
	 * Returns the successor of this node.
	 */
	public ImageNode getNext() {
		return next;
	}

	/**
	 * Changes the successor of this node.
	 */
	public void setNext(ImageNode newNext) {
		this.next = newNext;
	}

	/**
	 * Returns the number of nodes in the list started by this node.
	 * 
	 * Can be called from outside this class, and then switches between using
	 * the iterative or recursive private implementations of the method
	 * depending on the value of a boolean field in the SlideshowApp class.
	 *
	 * @return the number of nodes in the list starting at this node.
	 */
	public int count() {
		if (SlideshowApp.isRecursive) {
			return countRec();
		} else {
			return countIter();
		}
	}

	/**
	 * Returns the number of elements in the list starting from this node, using
	 * an iterative approach
	 *
	 * For the core part of the assignment.
	 * 
	 */
	private int countIter() {
		ImageNode itr = this;
		int count = 0;
		while (itr != null) {
			itr = itr.next;
			count++;
		}
		return count;
	}

	/**
	 * Returns the number of elements in the list starting from this node, using
	 * a recursive approach
	 *
	 * For the completion part of the assignment.
	 * 
	 */
	private int countRec() {
		return (next != null ? next.countRec() : 0) + 1;
	}

	/**
	 * Inserts a node after this node.
	 * 
	 * @param newNode
	 *            the node to be inserted
	 */

	public void insertAfter(ImageNode newNode) {
		newNode.setNext(this.getNext());
		this.setNext(newNode);
	}

	/**
	 * Inserts a new node before the cursor position
	 * 
	 * For the core part of the assignment.
	 * 
	 * Assumption: The cursor points to some node that is part of the list
	 * started by the successor of this node. In other words, the cursor cannot
	 * point to this node, but will point to one of the successors of this node.
	 * 
	 * @param newNode
	 *            the new node to be inserted
	 * @param cursor
	 *            the position before which the node needs to be inserted
	 * 
	 *            HINT: Make use of method 'nodeBefore' in order to find the
	 *            node before the cursor. HINT: Once you have the previous node,
	 *            you can make use of method 'insertAfter'.
	 *
	 */
	public void insertBefore(ImageNode newNode, ImageNode cursor) {
		if (next == cursor) {
			newNode.setNext(cursor);
			next = newNode;
		} else {
			if (next != null) {
				next.insertBefore(newNode, cursor);
			}
		}
	}

	/**
	 * Returns the node before the provided node.
	 * 
	 * Assumption: The provided node is one of the successors of this node.
	 * 
	 * @param target
	 *            the node whose predecessor is required
	 */

	public ImageNode nodeBefore(ImageNode target) {
		if (SlideshowApp.isRecursive) {
			return nodeBeforeRec(target);
		} else {
			return nodeBeforeIter(target);
		}

	}

	/**
	 * 
	 * Returns the node before the provided node, using an iterative approach
	 * 
	 * For the core part of the assignment.
	 * 
	 * Assumption: The provided node is one of the successors of this node.
	 *
	 * @param target
	 *            the node whose predecessor is required
	 */
	private ImageNode nodeBeforeIter(ImageNode target) {
		ImageNode itr = next;
		ImageNode last = this;
		while (itr != null) {
			if (target == itr) {
				return last;
			}
			last = itr;
			itr = itr.next;
		}
		return null;
	}

	/**
	 * Returns the node before the provided node, using a recursive approach
	 * 
	 * For the completion part of the assignment.
	 * 
	 * Assumption: The provided node is one of the successors of this node.
	 * 
	 * @param target
	 *            the node whose predecessor is required
	 */
	private ImageNode nodeBeforeRec(ImageNode target) {
		if (this.next == null)
			return null;

		if (target == this.next) {
			return this;
		} else {
			return next.nodeBeforeRec(target);
		}
	}

	/**
	 * Removes the node at the cursor, given a reference to the previous node
	 * 
	 * @param previous
	 *            the node preceding this node
	 */

	public void removeNodeUsingPrevious(ImageNode previous) {
		previous.setNext(this.getNext());
	}

	/**
	 * Reverses the order of the linked list starting at this node, so that the
	 * last node is now the first node, and and the second-to-last node is the
	 * second node, and so on
	 * 
	 * For the challenge part of the assignment.
	 * 
	 * This method should be called by method reverseRec in class Images.
	 * 
	 * @return the new first node
	 */

	public ImageNode reverseUsingPrevious(ImageNode previous) {
		if (next != null) {
			ImageNode front = next.reverseUsingPrevious(this);
			next = previous;
			return front;
		} else {
			next = previous;
			return this;
		}
	}
}
