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
    private ArrayList<Tile> tiles = new ArrayList<Tile>();
    
    public Rack ()
    {
    }
    
    public boolean on (double x , double y)
    {
        return false;
    }
    
    public int index (double x , double y)
    {
        return 0;
    }
    
    public Tile pickup (int pos)
    {
        return null;
    }
    
    public boolean place(Tile tile, int pos)
    {
        return false;
    }
    
    public void fill (Bag bag)
    {
        while (this.tiles.size() < 7)
        {
            this.tiles.add(bag.takeTile());
        }
    }

    public void draw()
    {
        int n = 0;
        for (Tile t : tiles )
        {
            t.draw(n*Tile.width , n*Tile.height);
            n++;
        }
    }
    
    public void reset()
    {
        this.tiles.clear();
    }
}
