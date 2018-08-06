// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/**
 * Class Images implements a list of images.
 * 
 * Each image is represented with an ImageNode object. The ImageNode objects
 * form a linked list.
 * 
 * An object of this class maintains the reference to the first image node and
 * delegates operations to image nodes as necessary.
 * 
 * An object of this class furthermore maintains a "cursor", i.e., a reference
 * to a location in the list.
 * 
 * The references to both first node and cursor may be null, representing an
 * empty collection.
 * 
 * @author Thomas Kuehne
 * @version 5/9/2013
 */

public class Images {
	private ImageNode head; // the first image node
	private ImageNode cursor; // the current point for insertion, removal, etc.

	/**
	 * Creates an empty list of images.
	 */
	public Images() {
		cursor = head = null;
	}

	/**
	 * Returns the fileName of the image at the current cursor position.
	 * 
	 * This method relieves clients of Images from knowing about image nodes and
	 * the 'getFileName()' method.
	 */
	public String getImageFileNameAtCursor() {
		// deal with an inappropriate call gracefully
		if (cursor == null)
			return ""; // the correct response would be to throw an exception.

		return cursor.getFileName();
	}

	/**
	 * Returns the current cursor position.
	 * 
	 * Used by clients that want to save the current selection in order to
	 * restore it after an iteration.
	 */
	public ImageNode getCursor() {
		return cursor;
	}

	/**
	 * Sets the cursor to a new node.
	 * 
	 * @param newCursor
	 *            the new cursor position
	 */
	public void setCursor(ImageNode newCursor) {
		cursor = newCursor;
	}

	/**
	 * Positions the cursor at the start
	 * 
	 * For the core part of the assignment.
	 */
	public void moveCursorToStart() {
		cursor = head;
	}

	/**
	 * Positions the cursor at the end
	 *
	 * For the core part of the assignment.
	 * 
	 * HINT: Consider the list could be empty.
	 */
	public void moveCursorToEnd() {
		while (cursor.getNext() != null) {
			cursor = cursor.getNext();
		}
	}

	/**
	 * Moves the cursor position to the right.
	 */
	public void moveCursorRight() {
		// is it impossible for the cursor to move right?
		if (cursor == null || cursor.getNext() == null)
			return;

		// advance the cursor
		cursor = cursor.getNext();
	}

	/**
	 * Moves the cursor position to the left.
	 * 
	 * Assumption: 'cursor' points to a node in the list!
	 */
	public void moveCursorLeft() {
		// is it impossible for the cursor to move left?
		if (head == null || cursor == head)
			return;

		// setup an initial attempt to a reference to the node before the
		// current cursor
		ImageNode previous = head;

		// while the node before the cursor has not been found yet, keep
		// advancing
		while (previous.getNext() != cursor) {
			previous = previous.getNext();
		}

		cursor = previous;
	}

	/**
	 * Returns the number of images
	 * 
	 * @return number of images
	 */
	public int count() {
		if (head == null) // is the list empty?
			return 0; // yes -> return zero

		return head.count(); // no -> delegate to linked structure
	}

	/**
	 * Adds an image after the cursor position
	 * 
	 * For the core part of the assignment.
	 * 
	 * @param imageFileName
	 *            the file name of the image to be added
	 * 
	 *            HINT: Consider that the current collection may be empty. HINT:
	 *            Create a new image node here and and delegate further work to
	 *            method 'insertAfter' of class ImageNode. HINT: Pay attention
	 *            to the cursor position after the image has been added.
	 * 
	 */
	public void addImageAfter(String imageFileName) {
		if (cursor != null) {
			cursor.setNext(new ImageNode(imageFileName, cursor.getNext()));
		} else {
			head = new ImageNode(imageFileName, null);
			cursor = head;
		}
	}

	/**
	 * Adds an image before the cursor position
	 * 
	 * For the completion part of the assignment.
	 * 
	 * @param imageFileName
	 *            the file name of the image to be added
	 * 
	 *            HINT: Create a new image node here and then 1. Consider that
	 *            the current collection may be empty. 2. Consider that the head
	 *            may need to be adjusted. 3. if necessary, delegate further
	 *            work to 'insertBefore' of class ImageNode. HINT: Pay attention
	 *            to the cursor position after the image has been added.
	 * 
	 */
	public void addImageBefore(String imageFileName) {
		if (head == null) {
			head = new ImageNode(imageFileName, null);
			cursor = head;
			return;
		}

		ImageNode before = (head != null ? head.nodeBefore(cursor) : null);
		if (before != null) {
			before.setNext(new ImageNode(imageFileName, cursor));
		} else {
			head = new ImageNode(imageFileName, head);
		}
	}

	/**
	 * Removes all images.
	 * 
	 * For the core part of the assignment.
	 */
	public void removeAll() {
		while (head != null) {
			remove();
		}
	}

	/**
	 * Removes an image at the cursor position
	 *
	 * For the core part of the assignment.
	 * 
	 * HINT: Consider that the list may be empty.
	 * 
	 * HINT: Handle removing at the very start of the list in this method and
	 * delegate the removal of other nodes by using method
	 * 'removeNodeUsingPrevious' from class ImageNode.
	 * 
	 * HINT: Make sure that the cursor position after the removal is correct.
	 */

	public void remove() {
		if (head == null)
			return;
		
		ImageNode before = head.nodeBefore(cursor);
		if (before != null) {
			before.setNext(cursor.getNext());
			if (cursor.getNext() != null) {
				cursor = cursor.getNext();
			} else {
				cursor = before;
			}
		} else if (cursor == head) {
			head = head.getNext();
			cursor = head;
		}
	}

	/**
	 * Reverses the list of images.
	 * 
	 * Can be called from outside this class, and then switches between using
	 * the iterative or recursive private implementations of the method
	 * depending on the value of a boolean field in the SlideshowApp class.
	 */

	public void reverseImages() {
		if (SlideshowApp.isRecursive) {
			reverseRec();
		} else {
			reverseIter();
		}
	}

	/**
	 * Reverses the order of the image list iteratively, so that the last node
	 * is now the first node, and and the second-to-last node is the second
	 * node, and so on
	 * 
	 * For the challenge part of the assignment.
	 * 
	 * HINT: Make sure there is something worth reversing first. HINT: You will
	 * have to use temporary variables. HINT: Don't forget to update the head of
	 * the list.
	 */
	private void reverseIter() {
		
		ImageNode prev = null , current = head, next = null;
		
		if (current != null)
			next = current.getNext();
		
		while (current != null) {
			
			next = current.getNext();
			current.setNext(prev);
			prev = current;
			current = next;
			
			
		}
		head = prev;
		
	}

	/**
	 * Reverses the order of this linked list by calling a recursive function,
	 * so that the last node is now the first node, and and the second-to-last
	 * node is the second node, and so on
	 * 
	 * For the challenge part of the assignment.
	 * 
	 * HINT: Make sure there is something worth reversing first. HINT: Delegate
	 * the work to a recursive method in class ImageNode. HINT: Make sure that
	 * head has the correct value afterwards.
	 */
	private void reverseRec() {
		this.head = this.head.reverseUsingPrevious(null);
	}
}
