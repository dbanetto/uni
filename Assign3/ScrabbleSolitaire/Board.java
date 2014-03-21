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

    /*# YOUR CODE HERE */
    /** Construct a new Board object */
    public Board(){
        /*# YOUR CODE HERE */
    }
    
    /** Is the position (x,y) on the board */
    public boolean on(double x, double y){
        /*# YOUR CODE HERE */
    }


    /**
     * Return the row/col corresponding to the point x,y.
     */
    public int[] rowCol(double x, double y){
        /*# YOUR CODE HERE */
    }

    /**
     * Pickup tile from the board, if the board position contains
     * a working tile and return it.
     */
    public Tile pickup(int row, int col){
        /*# YOUR CODE HERE */
    }

    /**
     * Place the tile on the board, if the board position is empty
     */
    public boolean place(Tile tile, int row, int col){
        /*# YOUR CODE HERE */
    }

    /**
     * Commit all the workingTiles to the board
     */
    public void commit(){
        /*# YOUR CODE HERE */
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
    }
                
            


    /**
     * Draw the board.
     * Assumes that the graphics pane has been cleared
     */
    public void draw(){
        /*# YOUR CODE HERE */
    }

    public void reset(){
        /*# YOUR CODE HERE */
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
