// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 103, Assignment 3
 * Name: David Barnett
 * Usercode: 300313764
 * ID: barnetdavi
 */

import ecs100.*;

import java.util.*;
import java.awt.Point;
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

	private Stack<ActionRecord> actions;

	// Constructors
	/**
	 * Construct a new Sokoban object and set up the GUI
	 */
	public Sokoban() {
		actions = new Stack<ActionRecord>();

		UI.addButton("New Level", this);
		UI.addButton("Restart", this);
		UI.addButton("Undo", this);
		UI.addButton("left", this);
		UI.addButton("up", this);
		UI.addButton("down", this);
		UI.addButton("right", this);

		UI.println("Put the boxes away.");
		UI.println("You may use keys (wasd or ijkl) but click on the graphics pane first");
		UI.setKeyListener(this);

		UI.setMouseListener(new UIMouseListener() {

			@Override
			public void mousePerformed(String action, double x, double y) {
				if (action.equals("clicked")) {
					
					int yb = (int) ((x - leftMargin) / (double) squareSize);
					int xb = (int) ((y - topMargin) / (double) squareSize);
					
					if (bounds(xb, 0, rows) && bounds(yb, 0, cols)) {
						UI.println("PATH FIND TO " + xb + "," + yb);
						pathFind(xb, yb);
					} else {
						UI.println("OUT OF BOUNDS " + xb + "," + yb + " "
								+ rows + "," + cols);
					}
				}
			}
		});

		initialiseMappings();
		load();
	}

	private void pathFind(int xtarget, int ytarget) {
		/*
		 * The Idea behind this path finding algorithm
		 * is to "point" back to the where the node came from.
		 * Repeating this until it has found the [T]arget tile.
		 * From there it can trace back the shortest route to the
		 * starting tile.
		 * 
		 * This works off the assumption that all tiles are equal
		 * and there is no extra cost to any tiles
		 */
		
		String[][] dir_board = new String[rows][cols];
		dir_board[xtarget][ytarget] = "T";
		dir_board[agentPos.row][agentPos.col] = "S";

		Queue<Point> qpts = new LinkedList<Point>();
		qpts.add(new Point(agentPos.row, agentPos.col));

		Point path_start = null;

		Map<Integer, String> xdir = new HashMap<Integer, String>();
		xdir.put(-1, "right");
		xdir.put(1, "left");

		Map<Integer, String> ydir = new HashMap<Integer, String>();
		ydir.put(-1, "down");
		ydir.put(1, "up");
		
		// make "nodes" to go out and search for the traget 
		
		while (!qpts.isEmpty() && path_start == null) {
			Point pt = qpts.poll();

			// Natural x/y
			int nx = pt.x, ny = pt.y;

			for (int x : new int[] { -1, 1 }) {
				if (nx + x == xtarget && ny == ytarget) {
					path_start = new Point(nx + x, ny);
					dir_board[nx + x][ny] = ydir.get(x);
					break;
				}
				
				if (bounds(nx + x, 0, rows) && bounds(ny, 0, cols)) {
					Square sqr = squares[nx + x][ny];
					if (sqr.free() && dir_board[nx + x][ny] == null) {
						dir_board[nx + x][ny] = ydir.get(x);
						qpts.add(new Point(nx + x, ny));
					}
				}
			}
			
			// Intermediate check
			if (path_start != null)
				break;

			for (int y : new int[] { -1, 1 }) {
				if (nx == xtarget && ny + y == ytarget) {
					path_start = new Point(nx, ny + y);
					dir_board[nx][ny + y] = xdir.get(y);
					break;
				}

				if (bounds(nx, 0, rows) && bounds(ny + y, 0, cols)) {
					Square sqr = squares[nx][ny + y];
					if (sqr.free() && dir_board[nx][ny + y] == null) {
						dir_board[nx][ny + y] = xdir.get(y);
						qpts.add(new Point(nx, ny + y));
					}
				}
			}
		}

		UI.println("Found a Path? "
				+ (path_start != null ? "Success!" : "Fail."));

		if (path_start == null)
			return;
		UI.println("Printing path map");
		for (int x = 0; x < rows; x++) {
			for (int y = 0; y < cols; y++) {
				String s = dir_board[x][y];
				UI.print(s != null ? s.charAt(0) : " ");
			}
			UI.print('\n');
		}
		
		// Get the moves from the map
		
		Stack<String> stack_dirs = new Stack<String>();
		int nx = path_start.x, ny = path_start.y;
		String dir = "T";
		
		UI.println("Filling movement stack");
		while (dir != "S") {
			dir = dir_board[nx][ny];
			if (dir == "S")
				break;
			
			switch (dir) {
			case "left":
				ny--;
				break;
			case "right":
				ny++;
				break;
			case "down":
				nx++;
				break;
			case "up":
				nx--;
				break;
			}

			stack_dirs.add(oppositeDirection(dir));
		}
		
		// Apply the moves to the Character
		
		UI.println("Playing out moves");
		while (!stack_dirs.isEmpty()) {
			String dr = stack_dirs.pop();
			if (!stack_dirs.isEmpty())
				agentDirection = stack_dirs.peek();
			
			move(dr, false);
			UI.sleep(25);
		}

	}

	private boolean bounds(int x, int l, int u) {
		return (x >= l && x < u);
	}

	/** Respond to button presses */
	public void buttonPerformed(String button) {
		if (button.equals("New Level")) {
			level = (level + 1) % maxLevels;
			load();
		} else if (button.equals("Restart")) {
			load();
		} else if (button.equals("Undo")) {
			if (!actions.isEmpty()) {
				ActionRecord rd = actions.pop();
				UI.printf("Undo a %s to the %s \n", (rd.isMove() ? "Move"
						: "Push"), rd.dir());
				if (rd.isMove()) {
					// Move Back
					move(oppositeDirection(rd.dir()), true);
					agentDirection = (!actions.empty() ? actions.peek().dir()
							: "left");
				} else if (rd.isPush()) {
					pull(oppositeDirection(rd.dir()));
					agentDirection = (!actions.empty() ? actions.peek().dir()
							: "left");
				}
			}
		} else
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
			move(dir, false);
		}
	}

	/** Move the agent into the new position (guaranteed to be empty) */
	public void move(String dir , boolean undo) {
		if (!undo)
			actions.add(new ActionRecord("move", dir));
		drawSquare(agentPos);
		agentPos = agentPos.next(dir);
		drawAgent();
		Trace.println("Move " + dir);
		UI.repaintGraphics();
	}

	/** Push: Move the agent, pushing the box one step */
	public void push(String dir) {
		actions.add(new ActionRecord("push", dir));
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
		actions = new Stack<>();
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
		UI.drawString("Agent : " + agentPos.row + "," + agentPos.col, 10, 10);
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
