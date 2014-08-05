// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 103, Assignment 3
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.util.*;
import java.io.*;

/**
 * Sokoban
 */

public class Sokoban implements UIButtonListener, UIKeyListener {

	// Fields
	private Square[][] squares; // the array describing the current warehouse.
	private int rows;
	private int cols;

	private Coord agentPos;
	private String agentDirection = "left";

	private final int maxLevels = 4;
	private int level = 0;

	private Map<Character, Square> squareMapping; // character in file to square
													// type
	private Map<Square, String> imageMapping; // square type to image of square
	private Map<String, String> agentMapping; // direction to image of worker
	private Map<String, String> keyMapping; // key string to direction

	// Constructors
	/**
	 * Construct a new Sokoban object and set up the GUI
	 */
	public Sokoban() {
		UI.addButton("New Level", this);
		UI.addButton("Restart", this);
		UI.addButton("left", this);
		UI.addButton("up", this);
		UI.addButton("down", this);
		UI.addButton("right", this);

		UI.println("Put the boxes away.");
		UI.println("You may use keys (wasd or ijkl) but click on the graphics pane first");
		UI.setKeyListener(this);

		initialiseMappings();
		load();
	}

	/** Respond to button presses */
	public void buttonPerformed(String button) {
		if (button.equals("New Level")) {
			level = (level + 1) % maxLevels;
			load();
		} else if (button.equals("Restart"))
			load();
		else
			doAction(button);
	}

	/** Respond to key actions */
	public void keyPerformed(String key) {
		doAction(keyMapping.get(key));
	}

	/**
	 * Move the agent in the specified direction, if possible. If there is box
	 * in front of the agent and a space in front of the box, then push the box.
	 * Otherwise, if there is anything in front of the agent, do nothing.
	 */
	public void doAction(String dir) {
		if (dir == null)
			return;
		agentDirection = dir;
		Coord newP = agentPos.next(dir); // where the agent will move to
		Coord nextP = newP.next(dir); // the place two steps over
		if (squares[newP.row][newP.col].hasBox()
				&& squares[nextP.row][nextP.col].free()) {
			push(dir);
		} else if (squares[newP.row][newP.col].free()) {
			move(dir);
		}
	}

	/** Move the agent into the new position (guaranteed to be empty) */
	public void move(String dir) {
		drawSquare(agentPos);
		agentPos = agentPos.next(dir);
		drawAgent();
		Trace.println("Move " + dir);
		UI.repaintGraphics();
	}

	/** Push: Move the agent, pushing the box one step */
	public void push(String dir) {
		drawSquare(agentPos);
		agentPos = agentPos.next(dir);
		drawAgent();
		Coord boxP = agentPos.next(dir);
		squares[agentPos.row][agentPos.col] = squares[agentPos.row][agentPos.col]
				.moveOff();
		squares[boxP.row][boxP.col] = squares[boxP.row][boxP.col].moveOn();
		drawSquare(boxP);
		Trace.println("Push " + dir);
		UI.repaintGraphics();
	}

	/**
	 * Pull: (useful for undoing a push in the opposite direction) move the
	 * agent in direction from dir, pulling the box into the agent's old
	 * position
	 */
	public void pull(String dir) {
		String opDir = oppositeDirection(dir);
		Coord boxP = agentPos.next(opDir);
		squares[boxP.row][boxP.col] = squares[boxP.row][boxP.col].moveOff();
		squares[agentPos.row][agentPos.col] = squares[agentPos.row][agentPos.col]
				.moveOn();
		drawSquare(boxP);
		drawSquare(agentPos);
		agentPos = agentPos.next(dir);
		agentDirection = opDir;
		drawAgent();
		Trace.println("Pull " + dir);
		UI.repaintGraphics();
	}

	/** Load a grid of squares (and agent position) from a file */
	public void load() {
		File f = new File("warehouse" + level + ".txt");
		if (f.exists()) {
			List<String> lines = new ArrayList<String>();
			try {
				Scanner sc = new Scanner(f);
				while (sc.hasNext())
					lines.add(sc.nextLine());
				sc.close();
			} catch (IOException e) {
				Trace.println("File error " + e);
			}

			rows = lines.size();
			cols = lines.get(0).length();

			squares = new Square[rows][cols];

			for (int row = 0; row < rows; row++) {
				String line = lines.get(row);
				for (int col = 0; col < cols; col++) {
					if (col >= line.length())
						squares[row][col] = Square.empty;
					else {
						char ch = line.charAt(col);
						if (squareMapping.containsKey(ch))
							squares[row][col] = squareMapping.get(ch);
						else {
							squares[row][col] = Square.empty;
							UI.printf("Invalid char: (%d, %d) = %c \n", row,
									col, ch);
						}
						if (ch == 'A')
							agentPos = new Coord(row, col);
					}
				}
			}
			draw();
		}
	}

	// Drawing

	private static final int leftMargin = 40;
	private static final int topMargin = 40;
	private static final int squareSize = 25;

	/** Draw the grid of squares on the screen, and the agent */
	public void draw() {
		UI.clearGraphics();
		// draw squares
		for (int row = 0; row < rows; row++)
			for (int col = 0; col < cols; col++)
				drawSquare(row, col);
		drawAgent();
		UI.repaintGraphics();
	}

	private void drawAgent() {
		UI.drawImage(agentMapping.get(agentDirection), leftMargin
				+ (squareSize * agentPos.col), topMargin
				+ (squareSize * agentPos.row), squareSize, squareSize, false);
	}

	private void drawSquare(Coord pos) {
		drawSquare(pos.row, pos.col);
	}

	private void drawSquare(int row, int col) {
		String imageName = imageMapping.get(squares[row][col]);
		if (imageName != null)
			UI.drawImage(imageName, leftMargin + (squareSize * col), topMargin
					+ (squareSize * row), squareSize, squareSize, false);
	}

	/**
	 * Return true iff the warehouse is solved - all the shelves have boxes on
	 * them
	 */
	public boolean isSolved() {
		for (int row = 0; row < rows; row++) {
			for (int col = 0; col < cols; col++)
				if (squares[row][col] == Square.shelf)
					return false;
		}
		return true;
	}

	/** Returns the direction that is opposite of the parameter */
	public String oppositeDirection(String dir) {
		if (dir.equals("right"))
			return "left";
		if (dir.equals("left"))
			return "right";
		if (dir.equals("up"))
			return "down";
		if (dir.equals("down"))
			return "up";
		return dir;
	}

	private void initialiseMappings() {
		// character in files to square type
		squareMapping = new HashMap<Character, Square>();
		squareMapping.put('.', Square.empty);
		squareMapping.put('A', Square.empty); // initial position of agent must
												// be an empty square
		squareMapping.put('#', Square.wall);
		squareMapping.put('S', Square.shelf);
		squareMapping.put('B', Square.box);

		// square type to image of square
		imageMapping = new HashMap<Square, String>();
		imageMapping.put(Square.empty, "empty.gif");
		imageMapping.put(Square.wall, "wall.gif");
		imageMapping.put(Square.box, "box.gif");
		imageMapping.put(Square.shelf, "shelf.gif");
		imageMapping.put(Square.boxOnShelf, "boxOnShelf.gif");

		// direction to image of worker
		agentMapping = new HashMap<String, String>();
		agentMapping.put("up", "agent-up.gif");
		agentMapping.put("down", "agent-down.gif");
		agentMapping.put("left", "agent-left.gif");
		agentMapping.put("right", "agent-right.gif");

		// key string to direction
		keyMapping = new HashMap<String, String>();
		keyMapping.put("i", "up");
		keyMapping.put("I", "up");
		keyMapping.put("k", "down");
		keyMapping.put("K", "down");
		keyMapping.put("j", "left");
		keyMapping.put("J", "left");
		keyMapping.put("l", "right");
		keyMapping.put("L", "right");

		keyMapping.put("w", "up");
		keyMapping.put("W", "up");
		keyMapping.put("s", "down");
		keyMapping.put("S", "down");
		keyMapping.put("a", "left");
		keyMapping.put("A", "left");
		keyMapping.put("d", "right");
		keyMapping.put("D", "right");
	}

	public static void main(String[] args) {
		new Sokoban();
	}
}
