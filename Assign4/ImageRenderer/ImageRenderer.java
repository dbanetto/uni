// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP102 Assignment 5
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;

/** Renders pnm images (pbm, pgm, or ppm) onto the graphics panel
    ppm images are the simplest possible colour image format.
*/


public class ImageRenderer implements UIButtonListener{

    public static final        int top = 20;   // top edge of the image
    public static final int left = 20;  // left edge of the image
    public static final        int pixelSize = 2;  

    public ImageRenderer(){
        UI.initialise();
        UI.addButton("Render", this);
        UI.addButton("Quit", this);
    }

    public void buttonPerformed(String b){
        UI.clearText();
        UI.clearGraphics();
        if (b.equals("Render")){
            String fname = UIFileChooser.open("Image file to render");
            if (fname != null){
                this.renderImage(fname);
            }
        }
        else if (b.equals("Quit")){
            UI.quit();
        }
    }

    /**
     * Renders a pnm image file.
     * Asks for the name of the file, then renders the image at position (left, top).
     * Each pixel of the image is rendered by a square of size pixelSize
     * The first three tokens (other than comments) are
     *    the magic number (P1, P2, or P3),
     *    number of columns, (integer)
     *    number of rows,  (integer)
     * ppm and pgm files then have 
     *    colour depth  (integer: range of possible color values)
     * The remaining tokens are the pixel values
     *  (0 or 1 for pbm, single integer for pgm; red, green, and blue integers for ppm)
     * There may be comments anywhere in the file, which start with # and go to the end of the line. Comments should be ignored.
     * The image may be "animated", in which case the file contains a sequence of images
     * (ideally, but not necessarily, the same type and size), which should be rendered
     * in sequence.
     * This method should read the magic number then call the appropriate method for rendering the rest of the image
    */                                
    public void renderImage(String fname){
        /*# YOUR CODE HERE */
    }

    
    public static void main(String[] args){
        UI.setImmediateRepaint(false);
        ImageRenderer im = new ImageRenderer();
        im.renderImage("image-bee.ppm");   // this is useful for testing.
    }
} 






