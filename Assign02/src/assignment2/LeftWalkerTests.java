package assignment2;

import java.io.*;
import maze.*;
import maze.gui.MazeWindow;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Here are some simple test cases.
 * You should add your own to get more confidence that it works!
 */
public class LeftWalkerTests {
	
	@Test
    public void maze01() {
		int[][] path = {{2,2},{1,2},{0,2},{0,1},{0,0}};
		checkTest("3,3\n41,1,5\n8,0,4\n10,2,22\n",path);
	}
	
	@Test
    public void maze02() {
		int[][] path = {{2,1},{2,0}};
		checkTest("3,3\n3,1,45\n8,8,28\n10,10,12",path);		
	}
	
	@Test
    public void maze03() {
		int[][] path = {{2,1},{2,0},{2,1},{2,2}};
		checkTest("3,3\n3,1,13\n8,8,28\n10,10,44",path);		
	}
	
	@Test
    public void maze04() {
		int[][] path = {{2,2},{2,1},{2,0},{1,0},{1,1},{1,2}};
		checkTest("3,3\n9,1,5\n8,8,12\n10,42,30",path);			
	}
	
	@Test
    public void maze05() {
		int[][] path = {{1,1},{1,0},{2,0},{2,1},{2,2},{1,2},{0,2},{0,1},{0,0}};
		checkTest("3,3\n41,1,5\n8,16,4\n10,2,6\n",path);
	}
	
	@Test
	public void maze06() {
		int[][] path = { { 2, 0 }, { 3, 0 }, { 4, 0 }, { 3, 0 }, { 3, 1 },
				{ 4, 1 }, { 4, 2 }, { 4, 3 }, { 4, 4 }, { 3, 4 }, { 2, 4 },
				{ 1, 4 }, { 0, 4 }, { 0, 3 }, { 0, 2 }, { 0, 1 } };

		checkTest(
				"5,5\n45,9,17,1,7\n8,0,0,0,5\n8,2,2,2,4\n8,3,3,3,4\n10,3,3,3,6",
				path);
	}
	
	@Test
	public void maze07() {
		// This maze is a trickier one, since it requires memorisation to solve.
		int[][] path = { {2,3},{3,3},{3,2},{3,1},{2,1},{1,1},{1,2},{1,3},{2,3},{2,4},{1,4},{0,4},{0,3},{0,2},{0,1},{0,0},{1,0},{2,0},{3,0},{4,0} };

		checkTest(
				"5,5\n9,1,1,1,5\n8,0,0,0,36\n8,0,15,0,4\n8,0,16,0,4\n10,2,2,2,6\n",
				path);
	}

	@Test
	public void maze08() {
		// This maze is a trickier one, since it requires memorisation to solve.
		int[][] path = { { 0, 6 }, { 0, 5 }, { 0, 4 }, { 1, 4 }, { 1, 3 },
				{ 0, 3 }, { 0, 2 }, { 1, 2 }, { 1, 3 }, { 2, 3 }, { 2, 2 },
				{ 3, 2 }, { 4, 2 }, { 4, 1 }, { 3, 1 }, { 2, 1 }, { 1, 1 },
				{ 0, 1 }, { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 }, { 4, 0 },
				{ 5, 0 }, { 6, 0 }, { 7, 0 }, { 8, 0 }, { 9, 0 }, { 9, 1 },
				{ 9, 2 }, { 9, 3 }, { 9, 4 } };

		checkTest(
				"10,10\n9,3,3,3,1,3,3,3,3,5\n10,3,3,3,0,3,3,3,7,12\n9,5,9,3,2,3,3,1,7,12\n10,0,2,3,3,3,1,2,7,12\n9,2,3,3,1,3,2,3,7,12\n12,11,3,3,2,3,3,1,5,44\n24,3,3,1,3,3,3,2,2,4\n12,9,1,2,3,3,3,3,7,12\n8,2,2,3,1,3,3,3,7,12\n10,3,3,3,2,3,3,3,3,6",
				path);
	}

	@Test
	public void maze09() {
		// This maze is a trickier one, since it requires memorisation to solve.
		int[][] path = { { 2, 4 }, { 2, 3 }, { 2, 4 }, { 3, 4 }, { 4, 4 },
				{ 4, 3 }, { 4, 2 }, { 4, 1 }, { 3, 1 }, { 2, 1 }, { 1, 1 },
				{ 0, 1 }, { 0, 2 }, { 0, 3 }, { 0, 4 }, { 0, 5 }, { 1, 5 },
				{ 2, 5 }, { 2, 4 }, { 2, 5 }, { 1, 5 }, { 0, 5 }, { 0, 4 },
				{ 0, 3 }, { 0, 2 }, { 0, 1 }, { 0, 0 }, { 1, 0 }, { 2, 0 },
				{ 3, 0 } };

		checkTest(
				"5,6\n9,1,1,1,37\n8,0,0,0,4\n8,15,15,15,4\n8,15,13,15,4\n8,15,16,0,4\n10,2,2,2,6",
				path);
	}

	@Test public void maze10() {
		
		int[][] path = { { 1, 3 }, { 2, 3 }, { 3, 3 }, { 3, 2 }, { 2, 2 },
				{ 1, 2 }, { 0, 2 }, { 0, 1 }, { 0, 0 }, { 1, 0 }, { 2, 0 },
				{ 3, 0 }, { 4, 0 }, { 4, 1 }, { 4, 2 }, { 4, 3 }, { 4, 4 },
				{ 3, 4 }, { 2, 4 }, { 1, 4 }, { 0, 4 }, { 0, 3 }, { 1, 3 },
				{ 0, 3 }, { 0, 4 }, { 1, 4 }, { 2, 4 }, { 3, 4 }, { 4, 4 },
				{ 4, 3 }, { 4, 2 }, { 4, 1 }, { 4, 0 }, { 3, 0 } };

		checkTest(
				"5,5\n9,3,1,1,5\n8,1,4,44,12\n10,2,2,4,12\n9,19,3,6,12\n10,3,2,3,6",
				path);
	}
	
	/**
     * The following method runs the LeftWalker and checks it against the
     * correct path. If there is any deviation the test fails.
     * 
     * @param inputMaze
     * @param correctPath
     */
	private void checkTest(String inputMaze, int[][] correctPath) {
		try {
			Board board = new Board(new StringReader(inputMaze));
			Walker walker = new LeftWalker();
			MazeWindow.getWindowAndShow(board);
			walker.solve(board);
			Path p = board.getPath();
			
			// First, need to generate the walker's path
			int[][] walkerPath = new int[p.getSteps()][2];
			Coordinate c;
			int idx = walkerPath.length-1;			
			while((c=p.pop()) != null) {				
				walkerPath[idx][0] = c.getX();
				walkerPath[idx][1] = c.getY();
				idx--;
			}
			
			// Now, check the walker's path was correct
			for (int i = 0; i != walkerPath.length; ++i) {
				if (i >= walkerPath.length || i >= correctPath.length
						|| walkerPath[i][0] != correctPath[i][0]
						|| walkerPath[i][1] != correctPath[i][1]) {
					fail("walker path is: " + pathString(walkerPath)
							+ ", correct path is: " + pathString(correctPath));
				}			
			}						
			
		} catch(IOException e) {
			// ensure the maximum possible path length, so the test will fail.
			fail("io exceotion - " + e.getMessage());
		}
	}
	
	private String pathString(int[][] path) {
		String r = "";
		boolean firstTime=true;
		for(int[] p : path) {
			if(!firstTime) {
				r+=",";
			}
			firstTime=false;
			r += "(" + p[0] + "," + p[1] + ")";
		}
		return r;
	}
}
