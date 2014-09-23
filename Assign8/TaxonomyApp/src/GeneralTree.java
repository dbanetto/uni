// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

import java.util.*;
import java.io.*;
import javax.swing.*;
import ecs100.*;

/**
 * GeneralTree represents a tree and uses GeneralTreeNode as a supporting data
 * structure.
 * 
 * @author Thomas Kuehne
 * @version 15/9/2013
 * 
 *          Based on code written by Stuart Marshall and Monique Damitio
 */
public class GeneralTree {
	// values for supporting drawing
	public static final double nodeRad = 20;
	private static final int levelSep = 60;

	// reference to the root node
	private GeneralTreeNode root;

	/**
	 * The initial GeneralTree contains no nodes, so the root is set to null to
	 * reflect this.
	 */
	public GeneralTree() {
		this.root = null;
	}

	/**
	 * Finds the node in the tree that contains the given string.
	 * 
	 * @param name
	 *            the name of the node to locate.
	 */
	private GeneralTreeNode findNode(String name) {
		if (root == null)
			return null;

		return root.findNode(name);
	}

	/**
	 * Adds a new node (with the data string stored in it) as a child to the
	 * node identified with the parentName string
	 * 
	 * CORE.
	 * 
	 * HINT: There are two cases to consider:
	 * 
	 * 1. The provided 'parentName' is null, indicating that a new root node is
	 * to be created that contains the old root as a child
	 * 
	 * 2. The parentName is not null, indicating that a new node needs to be
	 * created and added as a child to the node identified by 'parentName'. Make
	 * sure to use method 'addChild(...)' from GeneralTreeNode.
	 * 
	 * This method should do nothing if the 'parentName' is not null, but no
	 * node can be found with this name.
	 * 
	 * HINT: The new node should only be added into the tree if its name is
	 * unique, i.e., doesn't already appear in the tree.
	 * 
	 * HINT: Make sure to use method 'addChild' from class GeneralTreeNode.
	 * 
	 * @newName the name of the node to be added
	 * @param parentName
	 *            the name of the intended parent node
	 * 
	 */
	public void addNode(String newName, String parentName) {
		GeneralTreeNode proudParent = findNode(parentName);
		if (proudParent != null && findNode(newName) == null) {
			proudParent.addChild(new GeneralTreeNode(newName));
		}
	}

	/**
	 * Removes the node containing the target string
	 * 
	 * CORE.
	 * 
	 * The children of the node to be removed must become children of its
	 * parent.
	 * 
	 * Do nothing if the node is the root node of the entire tree, or if target
	 * node doesn't exist.
	 * 
	 * HINT: Make sure to make use of methods 'remove' and 'addChildrenFromNode'
	 * from class GeneralTreeNode.
	 * 
	 */
	public void removeNode(String targetName) {
		GeneralTreeNode troubledParent = findNode(targetName);
		troubledParent.getParent().addChildrenFromNode(troubledParent); // I cannot handle them, please take them
		troubledParent.remove(); // Good bye
	}

	/**
	 * Moves the subtree starting at the node containing 'targetName' to become
	 * a child of the destination node
	 * 
	 * COMPLETION.
	 * 
	 * Note that if the destination node is in anywhere in the subtree with the
	 * root 'targetNode' then no move operation must occur.
	 * 
	 * HINT: If you are struggling to implement the above test, make at least
	 * sure that there is no attempt to move the root of the tree.
	 * 
	 * HINT: Make sure to use both methods 'remove' and 'addChild' from class
	 * GeneralTreeNode, as moving means to remove at one place and to add at
	 * another place.
	 *
	 * @param targetName
	 *            the name of the node to be moved
	 * @param destinationName
	 *            the name of the destination node to which the node to be moved
	 *            should be added as a new child
	 */
	public void moveSubtree(String targetName, String destinationName) {
		GeneralTreeNode targetNode = this.findNode(targetName);
		GeneralTreeNode destinationNode = this.findNode(destinationName);
		
		// Stop from moving self onto own sub-tree
		if (!targetNode.contains(destinationNode)) {
			targetNode.remove();
			destinationNode.addChild(targetNode);
		}
	}

	/**
	 * Given two nodes names returns the string at a third node that is the
	 * closest common ancestor of the nodes identified by the two node names
	 * 
	 * CHALLENGE.
	 * 
	 * The closest common ancestor is the node that is the root of the smallest
	 * subtree that contains both the first two nodes. The closest common
	 * ancestor could even be one of the first two nodes identified by the
	 * parameters. Note that this can only return null if one of the targets
	 * doesn't exist, as the tree's root node is the last resort as a common
	 * ancestor to all nodes in the tree.
	 * 
	 * HINT: You may find it easier to implement the algorithm here completely,
	 * instead of trying to delegate to GeneralTreeNode.
	 * 
	 * @param target1
	 *            the (assumed unique) string in the first node.
	 * @param target2
	 *            the (assumed unique) string in the second node.
	 * @return the string data at the closest common ancestor node, or null if
	 *         one or both of the parameter's target nodes don't exist.
	 */
	public String findClosestCommonAncestor(String target1, String target2) {
		GeneralTreeNode t1 = findNode(target1);
		GeneralTreeNode t2 = findNode(target2);
		
		Stack<GeneralTreeNode> t1Parents = new Stack<>();
		Stack<GeneralTreeNode> t2Parents = new Stack<>();
		
		GeneralTreeNode ptr = t1;
		while (ptr != null) {
			t1Parents.add(ptr);
			ptr = ptr.getParent();
		}
		
		ptr = t2;
		while (ptr != null) {
			t2Parents.add(ptr);
			ptr = ptr.getParent();
		}
		
		ptr = null;
		while(!t1Parents.isEmpty() && !t2Parents.isEmpty()) {
			t1 = t1Parents.pop();
			t2 = t2Parents.pop();
			if (t1.equals(t2))
				ptr = t1;
		}
		
		return (ptr != null ? ptr.getName() : "null" );
	}

	/**
	 * Calculates locations for each node in the tree
	 * 
	 * CHALLENGE.
	 * 
	 * This below version, along with its supporting methods, does not do a nice
	 * job - it just lays out all the nodes on each level evenly across the
	 * width of the window. It also assumes that the depth of the tree is at
	 * most 100.
	 * 
	 * To complete the challenge stage, implement a visually more appealing
	 * version of the below locations calculation.
	 */
	private void calculateLocations() {
		int[] widths = new int[100];

		computeWidths(root, 0, widths);

		int[] separations = new int[100]; // separations between nodes at each
											// level

		for (int d = 0; d < 100 && widths[d] != 0; d++) {
			separations[d] = (UI.getCanvasWidth() - 20) / (widths[d] + 1);
		}

		int[] nextPos = new int[100]; // loc of next node at each level

		for (int d = 0; d < 100; d++) {
			if (widths[d] == 0)
				break;

			nextPos[d] = separations[d] / 2;
		}

		setLocations(root, 0, nextPos, separations);
	}

	/**
	 * Computes the number of nodes at each level of the tree, by accumulating
	 * the count in the widths array
	 */
	private void computeWidths(GeneralTreeNode node, int depth, int[] widths) {
		widths[depth]++;

		for (GeneralTreeNode child : node.getChildren()) {
			computeWidths(child, depth + 1, widths);
		}
	}

	/**
	 * Sets the location of each node at each level of the tree, using the depth
	 * and positions
	 */
	private void setLocations(GeneralTreeNode node, int depth, int[] nextPos,
			int[] separations) {
		node.setLocation(new Location(nextPos[depth], levelSep * depth
				+ levelSep / 2));
		nextPos[depth] += separations[depth];

		for (GeneralTreeNode child : node.getChildren())
			setLocations(child, depth + 1, nextPos, separations);
	}

	/*
	 * #******************************************************** No more methods
	 * to complete / modify beyond this point
	 * ********************************************************
	 */

	/**
	 * Prints the strings of all the nodes under the given target node
	 * (including the target node itself)
	 * 
	 * @param target
	 *            the name of the node to start printing from.
	 * 
	 * 
	 */
	public void printSubtreeFrom(String targetName) {
		// attempt to find the target node
		GeneralTreeNode targetNode = this.findNode(targetName);

		// does the node not exist?
		if (targetNode == null)
			return;

		// delegate to target node
		targetNode.printSubtree();
	}

	/**
	 * Prints the names of all the nodes in the path from the target node to the
	 * root of the entire tree
	 * 
	 */
	public void printPathToRootFrom(String targetName) {
		// attempt to find the target node
		GeneralTreeNode targetNode = this.findNode(targetName);

		// does the node not exist?
		if (targetNode == null)
			return;

		UI.println("Path to root: ");

		// delegate to targetNode
		targetNode.printPathToRoot();
	}

	/**
	 * Prints all the string data of all the nodes at the given depth.
	 * 
	 * Print nothing if the depth >= 0, if there are no nodes at that depth.
	 * 
	 * @param depth
	 *            the depth of the nodes that are to be printed. The root is at
	 *            depth 0.
	 */
	public void printAllAtDepth(int depth) {
		if (depth < 0) {
			UI.println("Invalid depth - cannot be negative");
			return;
		}

		if (root == null)
			return;

		// delegate to root node, providing both target and current depth
		root.printAllAtDepth(depth, 0);
	}

	/**
	 * Saves the whole tree in a file in a format that it can be loaded back in
	 * and reconstructed.
	 */
	public void save() {
		String fname = UIFileChooser.save("Filename to save text into");

		if (fname == null) {
			JOptionPane.showMessageDialog(null, "No file name specified");
			return;
		}

		try {
			PrintStream ps = new PrintStream(new File(fname));

			// then write to it with a recursive method.
			saveHelper(root, ps);
			ps.close();
		} catch (IOException ex) {
			UI.println("File Saving failed: " + ex);
		}
	}

	/**
	 * This helper and other methods pertaining to loading/saving could have
	 * been moved into GeneralTreeNode. Having them outside GeneralTreeNode
	 * shows you how you can add functionality without adding to
	 * GeneralTreeNode's public interface.
	 */
	private void saveHelper(GeneralTreeNode node, PrintStream ps) {
		if (node == null) {
			return;
		}
		// Print out the data string for this node, and how many child nodes
		// there are
		ps.println(node.getName() + " " + node.getChildren().size());

		for (GeneralTreeNode child : node.getChildren()) {
			saveHelper(child, ps);
		}
	}

	/**
	 * Constructs a new tree loaded from a file.
	 * 
	 * This code is not very sophisticated. It does not cope with white space in
	 * node data.
	 * 
	 * @param scan
	 *            The scanner connected to the input stream of the file to be
	 *            loaded in from.
	 */
	public void load(Scanner scan) {
		if (scan.hasNext()) {
			root = loadHelper(scan.next(), scan);
		}
		return;
	}

	private GeneralTreeNode loadHelper(String data, Scanner scan) {
		GeneralTreeNode node = new GeneralTreeNode(data);
		int numChildren = scan.nextInt();
		String junk = scan.nextLine();

		for (int i = 0; i < numChildren; i++) {
			GeneralTreeNode child = loadHelper(scan.next(), scan);
			node.addChild(child); // tell node to add a child.
		}
		return node;
	}

	/**
	 * Redraws the whole tree.
	 * 
	 * First step is to calculate all the locations of the nodes in the tree.
	 * Then traverse the tree to draw all the nodes and lines between parents
	 * and children.
	 */
	public void redraw() {
		UI.clearGraphics();

		if (root == null)
			return;

		calculateLocations();
		root.redrawSubtree();
	}
}
