package assignment2;

import java.io.*;
import java.awt.event.*;
import maze.*;
import maze.gui.*;

/**
 * main class for the program
 * @author ncameron
 *
 */
public class Main {

	/**
	 * The main method for this program
	 * @param args
	 */
	public static void main(String[] args) throws Exception {
		 
		String fileName = null; // name of input file to read
		int width = 25; // default width of board to create
		int height = 25; // default height of board to create
		Mode mode = Mode.LEFT;
		
		// ======================================================
		// ======== First, parse command-line arguments ========
		// ======================================================

		for (int i = 0; i != args.length; ++i) {
			if (args[i].startsWith("-")) {
				String arg = args[i];
				if (arg.equals("-file")) {
					fileName = args[++i];					
				} else if (arg.equals("-width")) {
					width = Integer.parseInt(args[++i]);
				} else if (arg.equals("-height")) {
					height = Integer.parseInt(args[++i]);
				} else if (arg.equals("-keyw")) {
					mode = Mode.KEY;
				} else if (arg.equals("-leftw")) {
					mode = Mode.LEFT;
				} else {
					throw new RuntimeException("Unknown option: " + args[i]);
				}
			}
		}

		
		// ======================================================
		// ======= Second, create the board to be searched ======
		// ======================================================

		Board board;
		if(fileName != null) {			
			board = new Board(new FileReader(fileName));
		} else {
			board = new Board(width,height);
		}

		// ======================================================
		// ====== Third, create the walker to walk the maze =====
		// ======================================================

		Walker walker = null;
		if (mode == Mode.RANDOM) {
			walker = new RandomWalker();
		} else if (mode == Mode.KEY) {
			walker = new KeyWalker();
		} else {
			// must be left walker --- only option remaining.
			walker = new LeftWalker();
		}	

		// ======================================================
		// ============== Fourth, show the GUI ==================
		// ======================================================
			
		// Initialise the GUI and put it on the screen
		MazeWindow.getWindowAndShow(board);
			
		// Now, register the key walker
		if(walker instanceof KeyListener) {
			MazeWindow.mainWindow.addKeyListener((KeyListener)walker);
		}
			
		// ======================================================
		// ================== Fifth, solve the maze =============
		// ======================================================
						
		long time = System.currentTimeMillis(); // record start time

		// solve the maze!
		walker.solve(board);
			
		time = System.currentTimeMillis() - time; // subtract start time
                                                        // from current time
		System.out.println("Maze solved by " + walker.getName() + " in " + time + "ms");
		System.out.println("Solution has " + board.getPath().getSteps() + " steps.");
			
	}	
}

enum Mode { RANDOM, KEY, LEFT }
