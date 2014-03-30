// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;
import java.awt.Color;
import java.util.*;
import java.io.*;


/**
   Tile
   Represents a single tile.
   Needs at least the name of the image file for drawing itself
   and the value of the tile.  It could store the letter on the tile also,
   though this is not used in this version of the game.

   Needs a
   - constructor
   - draw method, to draw the tile at a position x,y
   - method to return the value of the tile.
*/

public class Tile{
    /*# YOUR CODE HERE */
    private String letter;
    private int value;
    
    public static final int width = 40;
    public static final int height = 40;
    
    public Tile (String in , int value)
    {
        this.value = value;
        this.letter = in;
    }
    
    public String getLetter()
    {
        return this.letter;
    }
    
    public int getValue()
    {
        return this.value;
    }
    
    public String getFile()
    {
        return "tiles/" + this.letter + ".jpg";
    }
    
    public void draw(double x , double y)
    {
        UI.drawImage(this.getFile(), x, y);
        
    }
}
