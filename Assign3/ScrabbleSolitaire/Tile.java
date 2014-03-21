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
    
    public static final int width = 25;
    public static final int height = 25;
    
    public Tile (String in , int value)
    {
        this.value = value;
        this.letter = in;
    }
    
    public int getValue()
    {
        return this.value;
    }
    
    public void draw(double x , double y)
    {
        UI.setColor(Color.black);
        UI.drawRect(x, y, width, height);
        
        UI.setFontSize(14);
        UI.drawString(( this.letter.equals("blank") ? "" : this.letter), x + 5, y + 16);
        
        UI.setFontSize(8);
        UI.drawString(""+this.value, x + width - 5, y + 10);
        
    }
}
