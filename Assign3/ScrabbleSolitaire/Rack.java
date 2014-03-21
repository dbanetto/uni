// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for Assignment 7
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;

/**    
    The rack is represented by an array of tiles.
    The rack should be displayed with a rectangular border and each tile displayed
    by drawing the image of the tile at that place. Empty spaces in the rack 
    should be represented by nulls and displayed as empty.
    The user can select a position on the rack using the mouse. The selected tile
    (or empty space) should be highlighted with a border around it.

   Suggested methods:
     - constructor
     - boolean on(double x, double y) : is the point x,y on the rack
     - int index(double x, double y) : returns the index of the cell at the point x,y on the rack
     - Tile pickup(int pos) : pick up the tile at the index pos (null if no tile)
     - boolean place(Tile tile, int pos) : place tile at the index pos on the rack
          pushing tiles to the side if necessary. (return true if successful, and false if rack full)
     - void fill(Bag bag) :  fill all the space on the rack from the bag
     - void draw() :  draw the rack
     - void reset() : reset the rack to initial empty state.
    
 */

public class Rack{
    /*# YOUR CODE HERE */
    private Tile[] tiles = new Tile[7];
    
    //Offsets for drawing the Rack on screen
    private static final int rack_x_offset = 10;
    private static final int rack_y_offset = 620;
    
    public Rack ()
    {
    }
    
    public boolean on (double x , double y)
    {
        return index(x,y) != -1;
    }
    
    public int index (double x , double y)
    {
        UI.println("Finding Index @ " + x + " " + y);
        //Check if there is a chance if they could find an index
        if (y < rack_y_offset && x < rack_x_offset)
        {
            return -1;
        }
        
        for (int n = 0; n < 7; n++)
        {
           double posX = n*Tile.width + rack_x_offset + 2*n ;
           double posY = Tile.height + rack_y_offset - 1;
           double posXM = posX + Tile.width + 2;
           double posYM = posY + Tile.height + 2;
           
           if ( x > posX && x < posXM && y > posY && y < posYM )
           {
               return n;
           }
        }
        return -1;
    }
    
    public Tile pickup (int pos)
    {
        return this.tiles[pos];
    }
    
    public boolean place(Tile tile, int pos)
    {
        return false;
    }
    
    public void fill (Bag bag)
    {
        for (int i = 0; i < 7; i++)
        {
            if (tiles[i] == null) {
                this.tiles[i] = bag.takeTile();
           }
        }
    }

    public void draw()
    {
        for (int n = 0; n < 7; n++)
        {
            UI.setColor(Color.black);
            UI.drawRect(n*Tile.width + rack_x_offset + 2*n , Tile.height + rack_y_offset - 1
                        , Tile.width + 2, Tile.height + 2);
            tiles[n].draw(n*Tile.width + rack_x_offset + 2*n + 1, Tile.height + rack_y_offset);
        }
    }
    
    public void reset()
    {
        this.tiles = new Tile[7];
    }
}
