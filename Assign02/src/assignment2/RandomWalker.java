package assignment2;

import maze.Direction;
import maze.View;
import maze.Walker;

/**
 * A walker which randomly moves about the board.
 */
public class RandomWalker extends Walker {
	
	/**
	 * Creates a new random walker.
	 * @param name the name of the walker
	 */
	public RandomWalker() {
		super("Random");
	}

	public Direction move(View v) {
		int random = (int) (Math.random() * 4);
		switch(random) {
		case 0: return Direction.NORTH;
		case 1: return Direction.EAST;
		case 2: return Direction.SOUTH;
		default: return Direction.WEST;
		}
	}	
}
