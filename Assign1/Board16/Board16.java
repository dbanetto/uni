// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP 103 Assignment 1
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;

import java.awt.Color;
import java.util.Random;

public class Board16 {

    private final int LIMIT = 7; // to determine the ration of twos over fours (the closer to 10 the more twos)
    private final int TARGET = 16;

    private final int COLS;
    private int [] board;
    
    public Board16 (int size) {
        COLS = size;
        board = new int [COLS];
    }

    /** Return whether the magic target number has been achieved (greater than)
    [CORE]
     */
    public boolean hasReachedTarget() {
        /*# YOUR CODE HERE */
    	for (int a : board)
    		if (a == TARGET)
    			return true;
    	return false;
    }

    /** Return whether the game is over (true) or not (false) 
    If there is some room available, the game is not over.
    If there is no room available, need to check whether some adjacent tiles hold the same value
    [CORE]
     */
    public boolean isGameOver() {
        /*# YOUR CODE HERE */
    	for (int i = 0; i < board.length; i++)
    	{
    		if (i - 1 >= 0 && board[i] == board[i-1])
    			return false;
    		
    		if (i + 1 < board.length && board[i] == board[i+1])
    			return false;
    		
    		if (board[i] == 0)
    			return false;
    		else if (board[i] == TARGET)
    			return true;
    	}
    	
    	return false;
    }

    /** Return the number of empty tiles 
    An empty tile is one which holds the value 0
    [CORE]
     */
    private int numEmptyTiles() {
        int n = 0;
        for (int a : board)
    		if (a == 0)
    			n++;
        return n;
    }

    /** Insert a random number (either 2 or 4) at a randon empty tile.
    Note that 7/10 times the number should be 2.
    An empty tile is one which holds the value 0
    [CORE]
     */
    public void insertRandomTile() {
        Random rnd = new Random();
    	while (numEmptyTiles() != 0)
        {
        	int index = rnd.nextInt(COLS);
        	if (board[index] == 0)
        	{
        		board[index] = (rnd.nextDouble() > ((double)LIMIT/10.0) ? 4 : 2 );
        		break;
        	}
        }
    }

    /** Move the tiles left. 
    Each time 2 tiles with the same number touch, the number are added and the two tiles merge on 
    the left side. An empty tile is then added on the right hand side of the board.
    Examples: 
    2 2 4 2 will give 4 4 2 0 (the first 2 twos merging into a 4. Then the remaining
    4 and 2, and completing on the right with a 0)
    4 4 2 2 will give 8 4 0 0 (4 and 4 merge into a 8, 2 and 2 merge into a 4, 
    completing with zeros on the right)
    4 4 4 4 will give 8 8 0 0 (First two fours merge together, the last two fours merge together)
    1. Move all the non-empty tiles left, making sure that all the empty tiles are on the right
    2. From left to right merge any two tiles with the same number by adding them, discarding
    the second one, and adding an empty tile on the right of the board.
    [COMPLETION]
     */
    public void left() {
        /*# YOUR CODE HERE */
    	boolean change = false;
    	boolean[] addup = new boolean[COLS];
    		
    	for (int i = 0; i < board.length - 1; i++ )
	    	{
	    		if (board[i] == 0  && board[i + 1] != 0)
	    		{
	    			board[i] = board[i+1];
	    			board[i + 1 ] = 0;
	    			change = true;
	    			addup[i] = true;
	    		} else
	    		if (board[i] == board[i+1] && board[i] != 0 && !addup[i] )
	    		{
	    			board[i] = board[i+1] * 2;
	    			board[i + 1 ] = 0;

	    		}
	    	}
	    do {
	    	change = false;
	    	for (int i = 0; i < board.length - 1; i++ )
	    	{
	    		if (board[i] == 0  && board[i + 1] != 0)
	    		{
	    			board[i] = board[i+1];
	    			board[i + 1 ] = 0;
	    			change = true;
	    			addup[i] = true;
	    		}
	    	}
    	} while (change);
    }

    /** Move the tiles right. 
    Each time 2 tiles with the same number touch, the number are added and the two tiles merge on 
    the right side. An empty tile is then added on the left hand side of the board.
    Examples: 
    2 2 4 2 will give 0 4 4 2 (2 and 4 remains unchanged, then the last right most twos merging 
    into a 4, completing with a zero on the left.)
    4 4 2 2 will give 0 0 8 4 (2 and 2 merge into a 4, 4 and 4 merge into a 8)
    4 4 4 4 will give 0 0 8 8 (First two fours merge together, the last two fours merge together)
    1. Move all the non-empty tiles right, making sure that all the empty tiles are on the left
    2. From right to left merge any two tiles with the same number by adding them, discarding
    the second one, and adding an empty tile on the left of the board.
    [COMPLETION]
     */
    public void right() {
        /*# YOUR CODE HERE */
    	boolean change = false;
    	boolean[] addup = new boolean[COLS];
    		
    	for (int i = board.length - 1; i > 0; i-- )
	    	{
	    		if (board[i] == 0  && board[i - 1] != 0)
	    		{
	    			board[i] = board[i-1];
	    			board[i-1] = 0;
	    			change = true;
	    			addup[i] = true;
	    		} else
	    		if (board[i] == board[i-1] && board[i] != 0 && !addup[i] )
	    		{
	    			board[i] = board[i-1] * 2;
	    			board[i-1 ] = 0;

	    		}
	    	}
	    do {
	    	change = false;
	    	for (int i = board.length - 1; i > 0; i-- )
	    	{
	    		if (board[i] == 0  && board[i-1] != 0)
	    		{
	    			board[i] = board[i-1];
	    			board[i-1] = 0;
	    			change = true;
	    			addup[i] = true;
	    		}
	    	}
    	} while (change);
    }

    public String toString() {
        String ans = "  ";
        for (int col = 0; col < board.length; col++) {
            ans += board[col];
        }
        return ans;
    }

    // layout of the board
    private final int boardLeft = 80;    // left edge of the board
    private final int boardTop = 40;     // top edge of the board
    private final int tileSize = 50;     // width of tiles in the board

    public void redraw() {
        UI.clearGraphics();
        for (int col = 0; col < board.length; col++) {
            drawTile(col);
        }
        UI.repaintGraphics();
    }

    private void drawTile(int col) {
        int shiftBy = 3;
        double left = boardLeft+col*tileSize;
        double top = boardTop;

        // Fill the rectangle with a colour matching the value of the tile
        UI.setColor(getColor(board[col]));
        UI.fillRect(left,top,tileSize,tileSize);

        // Outline the rectangle
        UI.setColor(Color.black);
        UI.drawRect(left,top,tileSize,tileSize);

        // Display the number
        UI.setFontSize(20);
        if (board[col] == 0) return;
        if (board[col] >= 16) UI.setColor(Color.white);
        double x = left + tileSize * 0.3;
        double y = top + tileSize * 0.6;
        UI.drawString(""+board[col], x, y);
    }

    private Color getColor(int value) {
        switch (value) {
            case 0 : { return Color.white; }     
            case 2 : { return Color.gray; }    
            case 4 : { return Color.orange; }  
            case 8 : { return Color.red; }   
            case 16 : { return Color.cyan; }     
            case 32 : { return Color.blue; }
            case 64 : { return Color.green; }
            default: {return Color.black;}
        }
    }

}
