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
    	
    	return true;
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
    
    public void move(Direction dir)
    {
    	if (dir.equals(Direction.DOWN) || dir.equals(Direction.UP) )
    	{
    		moveVert(dir);
    	} else
    	{
    		moveHornz(dir);
    	}
    }
    
    public void moveVert(Direction dir)
    {
    	if (dir == Direction.LEFT || dir == Direction.RIGHT )
    		return;
    	boolean positive = true;
    	if (dir == Direction.DOWN)
    		positive = false;
    	
    	System.out.println(String.format("Moving %s", dir.toString() ));
    	
    	for (int x = 0; x < ROWS; x++)
    	{
	    	for (int y = (positive ? 0 : COLS-1); ( positive ? y < COLS : y >= 0); y+= (positive ? 1 : -1 ))
	    	{
    			System.out.println(String.format("Looking at %d @ %d,%d", board[x][y], x ,y));
    			if (board[x][y] == 0)
    			{
    				boolean found = false;
    				System.out.println(String.format("Finding %d,%d a value to steal", x ,y));
    				for (int i = y; ( positive ? i < COLS : i >= 0); i += (positive ? 1 : -1 ))
    				{
    					if (board[x][i] != 0)
    					{
    						System.out.println(String.format("Found new value %d @ %d,%d for %d,%d", board[x][i], x ,i, x ,y));
    						board[x][y] = board[x][i];
    						board[x][i] = 0;
    						found = true;
    						break;
    					}
    				}
    				if (!found)
    					break;
    			}
    			boolean colated = false;
    			if (board[x][y] != 0)
    			{
    				if ( positive ?  y-1 >= 0 : y+1 < COLS )
    				{
    					int n = (positive ? y - 1 :  y + 1 );
    					if (board[x][y] == board[x][n])
    					{
    						System.out.println(String.format("Merged %d,%d and %d,%d", x ,n, x ,y));
    						board[x][n] *= 2;
    						board[x][y] = 0;
    						colated = true;
    					}
    				}
    			}
    			
    			if (colated)
    			{
    				boolean found = false;
    				System.out.println(String.format("POST-MERGE: Finding a value to steal from %d,%d", x ,y));
    				for (int i = y; ( positive ? i < ROWS : i >= 0); i += (positive ? 1 : -1 ))
    				{
    					if (y != i && board[x][i] != 0)
    					{
    						System.out.println(String.format("POST-MERGE:Found new value %d @ %d,%d for %d,%d", board[i][y], i ,y, x ,y));
    						board[x][y] = board[x][i];
    						board[x][i] = 0;
    						found = true;
    						break;
    					}
    				}
    				if (!found)
    					break;
    			}
        	}
    	}
    	
    	System.out.println("Completed move");
    }
    
    public void moveHornz(Direction dir)
    {
    	if (dir == Direction.DOWN || dir == Direction.UP )
    		return;
    	boolean positive = true;
    	if (dir == Direction.RIGHT)
    		positive = false;
    	
    	System.out.println(String.format("Moving %s", dir.toString() ));
    	for (int y = 0; y < COLS; y++)
    	{
    		for (int x = (positive ? 0 : ROWS-1); ( positive ? x < ROWS : x >= 0); x += (positive ? 1 : -1 ))
        	{
    			System.out.println(String.format("Looking at %d @ %d,%d", board[x][y], x ,y));
    			if (board[x][y] == 0)
    			{
    				boolean found = false;
    				System.out.println(String.format("Finding %d,%d a value to steal", x ,y));
    				for (int i = x; ( positive ? i < ROWS : i >= 0); i += (positive ? 1 : -1 ))
    				{
    					if (board[i][y] != 0)
    					{
    						System.out.println(String.format("Found new value %d @ %d,%d for %d,%d", board[i][y], i ,y, x ,y));
    						board[x][y] = board[i][y];
    						board[i][y] = 0;
    						found = true;
    						break;
    					}
    				}
    				if (!found)
    					break;
    			}
    			boolean colated = false;
    			if (board[x][y] != 0)
    			{
    				if ( positive ?  x - 1 >= 0 : x + 1 < ROWS )
    				{
    					int n = (positive ?x - 1 :  x + 1 );
    					if (board[x][y] == board[n][y])
    					{
    						System.out.println(String.format("Merged %d,%d and %d,%d", n ,y, x ,y));
    						board[n][y] *= 2;
    						board[x][y] = 0;
    						colated = true;
    					}
    				}
    			}
    			
    			if (colated)
    			{
    				boolean found = false;
    				System.out.println(String.format("Finding %d,%d a value to steal", x ,y));
    				for (int i = x; ( positive ? i < ROWS : i >= 0); i += (positive ? 1 : -1 ))
    				{
    					if (x != i && board[i][y] != 0)
    					{
    						System.out.println(String.format("Found new value %d @ %d,%d for %d,%d", board[i][y], i ,y, x ,y));
    						board[x][y] = board[i][y];
    						board[i][y] = 0;
    						found = true;
    						break;
    					}
    				}
    				if (!found)
    					break;
    			}
        	}
    	}
    	System.out.println("Completed move");
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
        
        FontUtil.drawCenteredString((int)left, (int)top, ""+board[col][row], tileSize, tileSize);
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
            case 128 : { return Color.darkGray; }
            case 256 : { return Color.magenta; }
            case 512 : { return Color.pink; }
            case 1024 : { return Color.yellow; }
            default: {return Color.black;}
        }
    }

}
