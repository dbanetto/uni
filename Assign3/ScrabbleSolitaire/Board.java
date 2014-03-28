// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.awt.Color;
import java.util.*;
import java.io.*;
import java.awt.Point;

/**
   Scrabble Board
   methods:
     - constructor
     - boolean on(double x, double y) : is the point x,y on the board
     - int[] rowCol(double x, double y) : returns the row/col at the point x,y on the board
     - Tile pickup(int row, int col) : pick up the tile in the cell at row/col (null if no tile)
     - boolean place(Tile tile, int row, int col) : place tile at the cell at row/col on the board
                 (return true if successful, and false if unsuccessful (no space at row/col)
     - boolean validPlay() :  the working tiles constitute a valid play.
     - void commit() :  commit all the working tiles, if it is a valid play.
     - void draw() :  draw the board
     - void reset() : reset the board to initial empty state.
     
    NOTE: you MUST use these methods for this class so that we can run an automated test
    procedure to check your code (copy of it is given at the end of the class).
 */

public class Board{
    
    //Point is the row , col position
    private Tile[][] board = new Tile[15][15];
    private Map<Point,Tile> tmpboard = new HashMap<Point,Tile>();
    
    private Map<Point,Integer> specialTiles = new HashMap<Point,Integer>();    
    
    private static final int board_x_offset = 20;
    private static final int board_y_offset = 10;
    
    private boolean firstplay = true;
    
    /*# YOUR CODE HERE */
    /** Construct a new Board object */
    public Board(){
        /*# YOUR CODE HERE */
        reset();
    }
    
    /** Is the position (x,y) on the board */
    public boolean on(double x, double y){
        if (x > board_x_offset && y > board_y_offset)
        {
            if (x < board_x_offset + 15*Tile.width && x < board_y_offset + 15*Tile.height)
            {
                return true;
            }
        }
        
        return false;
    }


    /**
     * Return the row/col corresponding to the point x,y.
     */
    public int[] rowCol(double x, double y){
        if (on(x,y) == false)
        {
            return new int[] {-1 , -1};
        }
        
        for (int row = 0; row < 15; row++)
        {
            
            for (int col = 0; col < 15; col++)
            {
             double posX = board_x_offset + row*Tile.width;
             double posY = board_y_offset + col*Tile.height;
             double posXM = posX + Tile.width;
             double posYM = posY + Tile.height;
             
             if (x > posX && y > posY && x < posXM && y < posYM)
             {
                 int[] pos = new int[] { row , col };
                 return pos;
             }
            }
        }
        
        /*# YOUR CODE HERE */
        return new int[] {-1 , -1};
    }

    /**
     * Pickup tile from the board, if the board position contains
     * a working tile and return it.
     */
    public Tile pickup(int row, int col){
        /*# YOUR CODE HERE */
        Point pt = new Point(row,col);
        
        if (this.tmpboard.containsKey(pt))
        {
            Tile tmp = this.tmpboard.get(pt);
            this.tmpboard.remove(pt);
            return tmp;
        }
        UI.println("Nope. Did not pick anything @ " + row + "," + col);
        return null;
    }

    /**
     * Place the tile on the board, if the board position is empty
     */
    public boolean place(Tile tile, int row, int col){
        Point pt = new Point(row,col);
        //Check if tile is already in the tmp board
        if (this.tmpboard.containsKey(pt))
        {
            UI.println("Nope. Placing onto another tile @ " + row + "," + col);
            return false;
        }
        
        if (this.board[row][col] != null)
        {
            UI.println("Nope. Placing onto another tile @ " + row + "," + col);
            return false;
        }
        
        this.tmpboard.put(pt, tile);
        return true;
    }

    /**
     * Commit all the workingTiles to the board
     */
    public Boolean commit(){
        /*# YOUR CODE HERE */
        for (Point pt :  this.tmpboard.keySet())
        {
           this.board[(int)pt.getX()][(int)pt.getY()] = this.tmpboard.get(pt);
        }
        this.tmpboard.clear();
        
        if (firstplay)
            firstplay = false;
        
        return true;
    }

    /**
     * Returns true if the working tiles consitute a valid play:
     * The tiles must all be on a single line (row or column) and with no gaps.
     *  "No gaps" means that there must not be any empty cell between any pair
     *  of moveable tiles, though the movable tiles are not necessarily adjacent
     *  if there are any fixed tiles between the movable tiles.
     * At least one of the moveable tiles must be adjacent to a fixed tile,
     * unless it is the very first turn, in which case there are no fixed tiles.
     */
    public boolean validPlay(){
        /*# YOUR CODE HERE */
        int isHor = -1;
        Point inital = null;
        Point min = null;
        Point max = null;
        
        if (firstplay && this.tmpboard.size() < 2)
        {
            UI.println("On the first turn put down more than one");
            return false;
        }
        
        for (Point pt :  this.tmpboard.keySet())
        {
           //Get 1st block
           if (inital == null) {
                inital = new Point(pt);
                max = new Point(pt);
                min = new Point(pt);
                continue;
           }
           
           if (min.getX() > pt.getX())
           {
               min.setLocation(pt.getX(), min.getY());
           }
           if (min.getY() > pt.getY())
           {
               min.setLocation(min.getX(), pt.getY());
           }
           
           if (max.getX() < pt.getX())
           {
               max.setLocation(pt.getX(), max.getY());
           }
           if (max.getY() < pt.getY())
           {
               max.setLocation(max.getX(), pt.getY());
           }
           
           
           if (isHor == -1)
           {
               //Check if the X's changed 
               if (pt.getX() != inital.getX())
               {
                   isHor = 1;
               }
               
               //Y's 
               if (pt.getY() != inital.getY())
               {
                   //Make sure they are not doubling up in delta rows/coll
                   if (isHor != -1)
                   {
                       UI.println("second tile is Diagonal");
                       return false;
                   }
                   isHor = 2;
               }
               continue;
           }
           //Check if the Y's changed
           if (isHor == 1)
           {
               if (pt.getY() != inital.getY())
                   {
                       UI.println("Not all tiles are along the Y axis pt:" + pt.getY() + " int:" + inital.getY());
                       return false;
                   }
           }
           //Check if the X's has changes
           if (isHor == 2)
           {
               if (pt.getX() != inital.getX())
               {
                   UI.println("Not all tiles are along the X axis");
                   return false;
               }
           }
        }
        //No changes
        if (inital == null)
        {
            UI.println("No changes found");
            return false;
        }
        
        //Line checks
        //Check a horizontal line
        boolean touchesExisiting = false;
        if (isHor == 1)
        {
            if (min.getY() != max.getY())
                return false;
            int y = (int)min.getY();
            int pre = (int)min.getX() - 1;
            int post = (int)max.getX() + 1;
            
            for ( int i = pre; i <= post; i++ )
            {
                if (i < 0 || i > 15)
                    continue;
                
                if ( (this.tmpboard.containsKey(new Point(i,y)) == false && this.board[i][y] == null ) )
                {
                    if (i != pre && i != post) {
                        UI.println("Found a gap at " + i + "," + y + " pre:" + pre + " post:" + post );
                        return false;
                    }
                }
                if (this.board[i][y] != null)
                {
                    touchesExisiting = true;
                }
                 //Search for connections with old commits
                if (y-1 > 0)
                {
                    if (this.board[i][y-1] != null)
                    {
                        touchesExisiting = true;
                    }
                }
                if (y+1 < 15)
                {
                    if (this.board[i][y+1] != null)
                    {
                        touchesExisiting = true;
                    }
                }
            }

        }
        //Check a virtal line
        if (isHor == 2)
        {
            if (min.getX() != max.getX())
                return false;
            int x = (int)min.getX();
            int pre = (int)min.getY() - 1;
            int post = (int)max.getY() + 1;
            for ( int i =  pre; i <= post; i++ )
            {
                if (i < 0 || i > 15)
                    continue;
                if ( (this.tmpboard.containsKey(new Point(x,i)) == false && this.board[x][i] == null )  )
                {
                    if (i != pre && i != post) {
                        System.out.println("Found a gap at " + x + "," + i + " pre:" + pre + " post:" + post);
                        return false;
                    }
                }
                
                if (this.board[x][i] != null)
                {
                    touchesExisiting = true;
                }
                //Search for connections with old commits
                if (x-1 > 0)
                {
                    if (this.board[x-1][i] != null)
                    {
                        touchesExisiting = true;
                    }
                }
                if (x+1 < 15)
                {
                    if (this.board[x+1][i] != null)
                    {
                        touchesExisiting = true;
                    }
                }
            }
        }
        
        if (this.firstplay == false && touchesExisiting == false)
        {
            UI.println("Not touching another word");
            return false;
        }
        
        UI.println("Valid Play");
        return true;
    }

    /**
     * Draw the board.
     * Assumes that the graphics pane has been cleared
     */
    public void draw(){
        /*# YOUR CODE HERE */
        for (int row = 0; row < 15; row++)
        {
            UI.setColor(Color.black);
            UI.drawString(""+row,   board_x_offset - 15 , board_y_offset + row*Tile.height - 10);
            for (int col = 0; col < 15; col++)
            {   
             if (col == 0)
             {
                 UI.setColor(Color.black);
                 UI.setFontSize(10);
                 UI.drawString(""+ Character.toChars(row + 65)[0], board_x_offset + row*Tile.width, board_y_offset + col*Tile.height - 1);
                 
             }
                
                Color color = Color.black;
                UI.setColor(color);
                
                if (specialTiles.containsKey(new Point(row,col)))
                {
                    int mutli = specialTiles.get(new Point(row,col));
                    switch (mutli)
                    {
                        case (3):
                            color = Color.red;
                            break;
                        case (2):
                            color = Color.blue;
                            break;
                    }
                    UI.setColor(color);
                    UI.fillRect(board_x_offset + row*Tile.width, board_y_offset + col*Tile.height
                    , Tile.width, Tile.height);
                } else {
                    UI.drawRect(board_x_offset + row*Tile.width, board_y_offset + col*Tile.height
                        , Tile.width, Tile.height);
                }
               
                if (this.board[row][col] != null) {
                    this.board[row][col].draw( board_x_offset + row*Tile.width, board_y_offset + col*Tile.height );
                }
            }   
        }
        UI.setColor(Color.black);
        UI.drawString("15",   board_x_offset - 15 , board_y_offset + 15*Tile.height - 10);
        
        for (Point pt :  this.tmpboard.keySet())
        {
           this.tmpboard.get(pt).draw(board_x_offset + pt.getX()*Tile.width , board_y_offset + pt.getY()*Tile.height);
        }
    }

    public void reset(){
        /*# YOUR CODE HERE */
        firstplay = true;
        
        //Reset the boards
        this.tmpboard.clear();
        board = new Tile[15][15];
        
        //Add Special tiles
        //3 times
        specialTiles.put(new Point(0,0) , 3);
        specialTiles.put(new Point(7,0) , 3);
        specialTiles.put(new Point(14,0) , 3);
        
        specialTiles.put(new Point(0,7) , 3);
        
        specialTiles.put(new Point(5,1) , 3);
        specialTiles.put(new Point(9,1) , 3);
        
        specialTiles.put(new Point(1,5) , 3);
        specialTiles.put(new Point(5,5) , 3);
        specialTiles.put(new Point(9,5) , 3);
        specialTiles.put(new Point(13,5), 3);
        //Line of reflection
        
        //Line of reflection
        specialTiles.put(new Point(1,9) , 3);
        specialTiles.put(new Point(5,9) , 3);
        specialTiles.put(new Point(9,9) , 3);
        specialTiles.put(new Point(13,9), 3);
        
        specialTiles.put(new Point(5,13) , 3);
        specialTiles.put(new Point(9,13) , 3);
        
        specialTiles.put(new Point(14,7) , 3);
        
        specialTiles.put(new Point(0,14) , 3);
        specialTiles.put(new Point(7,14) , 3);
        specialTiles.put(new Point(14,14) , 3);
        
        //2 Times
        specialTiles.put(new Point(7,7) , 2);
        
        specialTiles.put(new Point(3,0) , 2);
        specialTiles.put(new Point(11,0) , 2);
        
        specialTiles.put(new Point(0,3) , 2);
        specialTiles.put(new Point(0,11) , 2);
        
        specialTiles.put(new Point(3,14) , 2);
        specialTiles.put(new Point(11,14) , 2);
        
        specialTiles.put(new Point(14,3) , 2);
        specialTiles.put(new Point(14,11) , 2);
        
        for (int i = 0; i < 4; i++)
        {
            specialTiles.put(new Point(1+i,1+i) , 2);
            specialTiles.put(new Point(1+i,13-i) , 2);
            specialTiles.put(new Point(13-i,1+i) , 2);
            specialTiles.put(new Point(13-i,13-i) , 2);
        }
        
        //Center Block
        specialTiles.put(new Point(6,6) , 2);
        specialTiles.put(new Point(8,6) , 2);
        specialTiles.put(new Point(6,8) , 2);
        specialTiles.put(new Point(8,8) , 2);
        
        //Tessilate The arrow bit
        //tans[0] is  the same as cos(angle)
        //trans[1] is the same as sin(angle)
        for (int[] trans : new int[][] 
            { new int[] { 1 , 0} , new int[] { 0 , 1} , new int[] { -1 , 0} , new int[] { 0 , -1 }  }
            )
        {
            specialTiles.put(new Point(7 + (int)( 4*trans[0] ), 7 + (int)( 4*trans[1] ) ) , 2);
            specialTiles.put(new Point(7 + (int)( 5*trans[0] + 1*trans[1] ), 7 + (int)( 5*trans[1] - 1*trans[0] ) ) , 2);
            specialTiles.put(new Point(7 + (int)( 5*trans[0] - 1*trans[1] ), 7 + (int)( 5*trans[1] + 1*trans[0] ) ) , 2);
        }
        UI.println("Board Reset");
    }


    //====================================================================
    /**
     * Tests the reset, place, pick, commit, and validPlay methods
     * by putting tiles on the board.
     * Doesn't draw anything.
     */

    public static void testValid(){
        Board b = new Board();
        Tile t = new Tile("A", 1);
        System.out.println("Testing tiles in a row");
        //place tiles in a row 
        b.place(t, 2, 2);
        b.place(t, 2, 3);
        b.place(t, 2, 4);
        if (!b.validPlay()) {System.out.println("2/2, 2/3, 2/4 should be valid");}
        b.place(t, 2, 6);
        if (b.validPlay()) {System.out.println("2/2, 2/3, 2/4, 2/6 should NOT be valid");}
        b.place(t, 2, 5);
        if (!b.validPlay()) {System.out.println("2/2, .. 2/6 should be valid");}
        b.commit();

        System.out.println("Testing tiles in a disconnected row");
        b.place(t, 5, 4);
        b.place(t, 5, 5);
        b.place(t, 5, 6);
        if (b.validPlay()) {System.out.println("disconnected 5/4, 5/5/, 5/6 should NOT be valid");}
        System.out.println("Testing tiles in an L shape");
        b.place(t, 4, 6);
        b.place(t, 3, 6);
        if (b.validPlay()) {System.out.println("5/4, 5/5, 5/6, 4/6, 3/6 should NOT be valid");}
        b.pickup(5, 4);
        b.pickup(5, 5);
        if (!b.validPlay()) {System.out.println("5/6, 4/6, 3/6 should be valid");}


        System.out.println("Testing tiles in a column");
        b.reset();
        b.place(t, 2, 2);
        b.place(t, 3, 2);
        b.place(t, 4, 2);
        if (!b.validPlay()) {System.out.println("2/2, 3/2, 4/2 should be valid");}
        b.place(t, 6, 2);
        if (b.validPlay()) {System.out.println("2/2, 3/2, 4/2, 6/2 should NOT be valid");}
        b.place(t, 5, 2);
        if (!b.validPlay()) {System.out.println("2/2, .. 6/2 should be valid");}
        b.commit();

        System.out.println("Testing tiles in a disconnected column");
        b.place(t, 4, 5);
        b.place(t, 5, 5);
        b.place(t, 6, 5);
        if (b.validPlay()) {System.out.println("disconnected 4/5, 5/5/, 6/5 should NOT be valid");}
        b.place(t, 6, 4);
        b.place(t, 6, 3);
        if (b.validPlay()) {System.out.println(" 4/5, 5/5, 6/5, 6/4, 6/3 should NOT be valid");}
        b.pickup(4, 5);
        b.pickup(5, 5);
        if (!b.validPlay()) {System.out.println("6/5, 6/4, 6/3 should be valid");}

        System.out.println("Testing column connected at ends and side");
        b.reset();
        b.place(t, 10, 5);
        b.commit();
        b.place(t, 7, 5);
        b.place(t, 8, 5);
        b.place(t, 9, 5);
        if (!b.validPlay()) {System.out.println("7,8,9/5 should be valid, given 10/5");}
        b.reset();
        b.place(t, 10, 5);
        b.commit();
        b.place(t, 11, 5);
        b.place(t, 12, 5);
        b.place(t, 13, 5);
        if (!b.validPlay()) {System.out.println("11,12,13/5 should be valid, given 10/5");}
        b.reset();
        b.place(t, 10, 5);
        b.commit();
        b.place(t, 9, 6);
        b.place(t, 10, 6);
        b.place(t, 11, 6);
        if (!b.validPlay()) {System.out.println("9,10,11/6 should be valid, given 10/5");}

        System.out.println("Testing row connected at ends and side");
        b.reset();
        b.place(t, 5, 10);
        b.commit();
        b.place(t, 5, 7);
        b.place(t, 5, 8);
        b.place(t, 5, 9);
        if (!b.validPlay()) {System.out.println("5/7,8,9 should be valid, given 5/10");}
        b.reset();
        b.place(t, 5, 10);
        b.commit();
        b.place(t, 5, 11);
        b.place(t, 5, 12);
        b.place(t, 5, 13);
        if (!b.validPlay()) {System.out.println("5/11,12,13 should be valid, given 5/10");}
        b.reset();
        b.place(t, 5, 10);
        b.commit();
        b.place(t, 6, 9);
        b.place(t, 6, 10);
        b.place(t, 6, 11);
        if (!b.validPlay()) {System.out.println("6/9,10,11 should be valid, given 5/10");}


        System.out.println("Testing column spanning fixed tiles");
        b.reset();
        b.place(t, 6, 5);
        b.place(t, 9, 5);
        b.commit();
        if (b.validPlay()) {System.out.println("no working tiles should NOT be valid");}
        
        b.place(t, 4, 5);
        b.place(t, 5, 5);
        b.place(t, 7, 5);
        if (!b.validPlay()) {System.out.println("4,5,7/5 should be valid, given 6/5");}
        b.place(t, 10, 5);
        if (b.validPlay()) {System.out.println("4,5,7,10/5, should NOT be valid, given 6,9/5");}
        b.place(t, 8, 5);
        if (!b.validPlay()) {System.out.println("4,5,7,8,10/5, should be valid, given 6,9/5");}
        b.reset();
        b.place(t, 6, 5);
        b.commit();
        b.place(t, 3, 5);
        b.place(t, 4, 5);
        b.place(t, 7, 5);
        b.place(t, 8, 5);
        if (b.validPlay()) {System.out.println("3,4,7,8/5, should NOT be valid, given 6/5");}

        System.out.println("Testing row spanning fixed tiles");
        b.reset();
        b.place(t, 5, 6);
        b.place(t, 5, 9);
        b.commit();
        if (b.validPlay()) {System.out.println("no working tiles should NOT be valid");}
        
        b.place(t, 5, 4);
        b.place(t, 5, 5);
        b.place(t, 5, 7);
        if (!b.validPlay()) {System.out.println("5/4,5,7 should be valid, given 5/6");}
        b.place(t, 5, 10);
        if (b.validPlay()) {System.out.println("5/4,5,7,10, should NOT be valid, given 5/6,9");}
        b.place(t, 5, 8);
        if (!b.validPlay()) {System.out.println("5/4,5,7,8,10, should be valid, given 5/6,9");}
        b.reset();
        b.place(t, 5, 6);
        b.commit();
        b.place(t, 5, 3);
        b.place(t, 5, 4);
        b.place(t, 5, 7);
        b.place(t, 5, 8);
        if (b.validPlay()) {System.out.println("5/3,4,7,8, should NOT be valid, given 5/6");}


        System.out.println("Tests all done");
    }



}
