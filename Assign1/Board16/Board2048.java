import ecs100.*;

import java.awt.Color;
import java.util.Random;

public class Board2048 {
	private int TARGET = 2048;
	private int LIMIT = 9;
	
	private final int COLS;
	private final int ROWS;
    private int [][] board;
    
    public Board2048 (int size) {
        COLS = size;
        ROWS = size;
        board = new int [ROWS][COLS];
    }

    public boolean hasReachedTarget() {
        /*# YOUR CODE HERE */
    	for (int[] rows : board)
    		for (int cell : rows)
    			if (cell == TARGET)
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
    	for (int x = 0; x < board.length; x++)
    	{
    		for (int y = 0; y < board[x].length; y++)
    		{
    			if (board[x][y] == 0)
    				return false;
    			
    			if (x + 1 < board.length && board[x][y] == board[x+1][y])
        			return false;
    			if (x - 1 >= 0 && board[x][y] == board[x-1][y])
        			return false;
    			
    			if (y + 1 < board[y].length && board[x][y] == board[x][y+1])
        			return false;
    			if (y - 1 >= 0 && board[x][y] == board[x][y-1])
        			return false;
    			
    			if (board[x][y] == TARGET)
    				return true;
    		}
    	}
    	
    	return false;
    }

    /** Return the number of empty tiles 
    An empty tile is one which holds the value 0
    [CORE]
     */
    private int numEmptyTiles() {
        int n = 0;
        for (int[] rows : board)
    		for (int cell : rows)
    			if (cell == 0)
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
        	int y = rnd.nextInt(COLS);
        	int x = rnd.nextInt(ROWS);
        	if (board[x][y] == 0)
        	{
        		board[x][y] = (rnd.nextDouble() > ((double)LIMIT/10.0) ? 4 : 2 );
        		break;
        	}
        }
    }

    public void left() {
        /*# YOUR CODE HERE */
    	boolean change = false;
    	boolean[] addup = new boolean[COLS];
    	for (int y = 0; y < COLS; y++)
    	{
    		for (int i = 0; i < board.length - 1; i++ ) {
	    		if (board[i][y] == 0  && board[i + 1][y] != 0) {
	    			board[i][y] = board[i+1][y];
	    			board[i + 1 ][y] = 0;
	    			change = true;
	    			addup[i] = true;
	    		} else if (board[i][y] == board[i+1][y] && board[i][y] != 0 && !addup[i] )	{
	    			board[i][y] = board[i+1][y] * 2;
	    			board[i+1][y] = 0;
			    }
		    }
	    	
		    do {
		    	change = false;
		    	for (int i = 0; i < board.length - 1; i++ )
		    	{
		    		if (board[i][y] == 0  && board[i+1][y] != 0)
		    		{
		    			board[i][y] = board[i+1][y];
		    			board[i+1][y] = 0;
		    			change = true;
		    		}
		    	}
	    	} while (change);
    	}
    }

    public void right() {
        /*# YOUR CODE HERE */
    	boolean change = false;
    	boolean[] addup = new boolean[COLS];
    	for (int y = 0; y < COLS; y++) {
	    	for (int i = board.length - 1; i > 0; i-- )
		    	{
		    		if (board[i][y] == 0  && board[i-1][y] != 0)
		    		{
		    			board[i][y] = board[i-1][y];
		    			board[i-1][y] = 0;
		    			change = true;
		    			addup[i] = true;
		    		} else
		    		if (board[i][y] == board[i-1][y] && board[i][y] != 0 && !addup[i] )
		    		{
		    			board[i][y] = board[i-1][y] * 2;
		    			board[i-1 ][y] = 0;
	
		    		}
		    	}
		    do {
		    	change = false;
		    	for (int i = board.length - 1; i > 0; i-- )
		    	{
		    		if (board[i][y] == 0  && board[i-1][y] != 0)
		    		{
		    			board[i][y] = board[i-1][y];
		    			board[i-1][y] = 0;
		    			change = true;
		    		}
		    	}
	    	} while (change);
	    }
    }

    public void up() {
        /*# YOUR CODE HERE */
    	boolean change = false;
    	boolean[] addup = new boolean[COLS];
    	for (int x = 0; x < ROWS; x++)
    	{
    		for (int i = 0; i < board.length - 1; i++ ) {
	    		if (board[x][i] == 0  && board[x][i+1] != 0) {
	    			board[x][i] = board[x][i+1];
	    			board[x][i+1] = 0;
	    			change = true;
	    			addup[i] = true;
	    		} else if (board[x][i] == board[x][i+1] && board[x][i] != 0 && !addup[i] )	{
	    			board[x][i] = board[x][i+1] * 2;
	    			board[x][i+1] = 0;
			    }
		    }
	    	
		    do {
		    	change = false;
		    	for (int i = 0; i < board.length - 1; i++ )
		    	{
		    		if (board[x][i] == 0  && board[x][i+1] != 0)
		    		{
		    			board[x][i] = board[x][i+1];
		    			board[x][i+1] = 0;
		    			change = true;
		    		}
		    	}
	    	} while (change);
    	}
    }
    
    public void down() {
        /*# YOUR CODE HERE */
    	boolean change = false;
    	boolean[] addup = new boolean[COLS];
    	for (int x = 0; x < ROWS; x++) {
	    	for (int i = board.length - 1; i > 0; i-- )
		    	{
		    		if (board[x][i] == 0  && board[x][i-1] != 0)
		    		{
		    			board[x][i] = board[x][i-1];
		    			board[x][i-1] = 0;
		    			change = true;
		    			addup[i] = true;
		    		} else
		    		if (board[x][i] == board[x][i-1] && board[x][i] != 0 && !addup[i] )
		    		{
		    			board[x][i] = board[x][i-1] * 2;
		    			board[x][i-1] = 0;
	
		    		}
		    	}
		    do {
		    	change = false;
		    	for (int i = board.length - 1; i > 0; i-- )
		    	{
		    		if (board[x][i] == 0 && board[x][i-1] != 0)
		    		{
		    			board[x][i] = board[x][i-1];
		    			board[x][i-1] = 0;
		    		}
		    	}
	    	} while (change);
	    }
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
        UI.clearGraphics(false);
        for (int row = 0; row < ROWS; row++) {
	        for (int col = 0; col < COLS; col++) {
	            drawTile(row,col);
	        }
        }
        UI.repaintGraphics();
    }

    private void drawTile(int row, int col) {
        int shiftBy = 3;
        double left = boardLeft +col*tileSize;
        double top = boardTop + row * tileSize;

        // Fill the rectangle with a colour matching the value of the tile
        UI.setColor(getColor(board[col][row]));
        UI.fillRect(left,top,tileSize,tileSize);

        // Outline the rectangle
        UI.setColor(Color.black);
        UI.drawRect(left,top,tileSize,tileSize);

        // Display the number
        UI.setFontSize(20);
        if (board[col][row] == 0) return;
        if (board[col][row] >= TARGET) UI.setColor(Color.white);
        
        FontUtil.invertCenteredString((int)left, (int)top, ""+board[col][row], tileSize, tileSize);
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
