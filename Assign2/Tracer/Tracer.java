// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment 
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;
import javax.swing.JColorChooser;



/** Tracer
 *  The program displays an image on the screen consisting of a tangle of
 *  microtubules.  
 *  It will enable the user to measure the lengths of the microtubles 
 *  by tracing out polylines (sequences of connected straight line segments)
 *  over the image.
 *  After the user clicks the "Start Line" button, the program will clear all lines,
 *   redisplay the image, and get ready to start a new line
 *  As the user clicks on points on the image, it will build up a "polyline" connecting
 *   the points.  For the first point on the line, it just draws a dot.
 *   For each remaining point the user clicks on, the program will draw
 *   a new line segment from the previous point.
 *  It also keeps track of the total length of the line, adding the length of the
 *   new segment to the total on each click.
 *  When the user clicks the "Line Length" button, it will print out the total length of the line.
 *  When the user clicks the "Choose Image" button, it will allow the user to select a different
 *   image, and will restart the line.
 *
 *  You will need
 *  - fields to record the previous point on the polyline, and the length so far.
 *  - a constructor to set up the GUI (including the mouse listener)
 *  - methods to respond to events (buttons and mouse)
 *  - possibly additional "helper" methods.
 */
public class Tracer implements UIButtonListener, UIMouseListener{
    // Fields
    private String imageName = "image01.jpg";     // the current image that will be displayed

    // Other fields to record where the previous point on the polyline was.
    // (negative if there was no previous point), and the length of the line

    /*# YOUR CODE HERE */


    // Constructor
    /** 
     *  Construct a new Tracer object and set up the GUI
     */
    public Tracer(){
        /*# YOUR CODE HERE */
    }

    // GUI Methods

    /** Respond to button presses */
    public void buttonPerformed(String button){
        /*# YOUR CODE HERE */
    }

    /** Respond to mouse events, particularly to "released" */
    public void mousePerformed(String action, double x, double y) {
        /*# YOUR CODE HERE */
    }


    // other methods: you don't have to define this method, but it may be useful

    /**
     * Clear the screen, redisplay the image, and get ready to start a new line.
     * Sets the values of the fields storing the current point to -1
     */
    private void startLine(){
        /*# YOUR CODE HERE */
    }

    /*# END OF YOUR CODE */

    // Main
    public static void main(String[] arguments){
        new Tracer();
    }        


}
