// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for Assignment 7
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;

/**
    The bag is represented by a list of tiles.
    The bag should be initialised to have the standard distribution of tiles:

    2 blank tiles                                      0 points
    Ex12, Ax9, Ix9, Ox8, Nx6, Rx6, Tx6, Lx4, Sx4, Ux4  1 point 
    Dx4, Gx3                                            2 points
    Bx2, Cx2, Mx2, Px2                                    3 points
    Fx2, Hx2, Vx2, Wx2, Yx2                            4 points
    Kx1                                                    5 points
    Jx1, Xx1                                            8 points
    Qx1, Zx1                                            10 points
 */

public class Bag{
    /*# YOUR CODE HERE */
    private List<Tile> tiles = new ArrayList<Tile>();
    
    private Random rnd;
    
    public Bag(int seed)
    {
        rnd = new Random(seed);
    }
    
    public void load(String file)
    {
        
        try 
        {
            //Example code from: http://stackoverflow.com/questions/5868369/how-to-read-a-large-text-file-line-by-line-using-java
            BufferedReader br = new BufferedReader(new FileReader(file));
            String line;
            UI.println("Loading tiles from " + file);
            while ((line = br.readLine()) != null) {
               int value = 0;
               String name= "";
                
               String[] bits = line.split(" ");
               try {
                   value = Integer.parseInt(bits[1]);
                   name = bits[0];
                   this.tiles.add(new Tile(name, value));
                   //UI.println("Tile loaded : " + name + " " + value);
               } catch (Exception ex)
               {
                   UI.println("Error while reading : " + line + "\n" + ex.toString());
               }
            }
            UI.println("Completed loading tiles");
            br.close();
        } catch (FileNotFoundException ex)
        {
            UI.println("File not found " + file);
        } catch (IOException ex)
        {
            UI.println("IO Error");
        }
    }
    
    public boolean isEmpty()
    {
        return (this.tiles.size() == 0 ? true : false );
    }
    
    public Tile takeTile()
    {
        int rndt = rnd.nextInt(this.tiles.size());
        Tile out = tiles.get(rndt);
        this.tiles.remove(rndt);
        return out;
    }
}
