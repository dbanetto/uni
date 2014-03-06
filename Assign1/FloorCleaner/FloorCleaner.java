// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment 1  
 * Name:
 * Usercode:
 * ID:
 */

//import java.util.*;
import ecs100.*;
import java.awt.Color;


/** Makes a dirty floor (a grey rectangle), and then makes a robot floor
    cleaner run around on the floor to clean it all up.
    The robot will go forward one unit on each step in its current
    direction, and erase the "dirt" (by erasing where it was, moving
    one step, and redrawing itself).
    When it is about to take a step, the program must check that
    it won't go over edges of the floor, and change its direction
    to a new random direction if it would go over the edge.

    Hints:
    - You can use Math.random() to create a random number (between 0.0 and 1.0)
    - If the robot needs to move a distance s in the direction d, then it can
      work out the amounts to move in the x and y directions respectively using
      basic trigonometry:
        s * Math.cos(d * Math.PI/180)   and   s * Math.sin(d * Math.PI/180)
      (assuming that d is measured in degrees from 0 to 360)
    - Write one method with the main loop, and a separate method for
      drawing the floor cleaner. It should show the direction the
      floor cleaner is heading in some way. 
 */

public class FloorCleaner{

    /**
     * cleanFloor is the main simulation loop.
     */
    /*# YOUR CODE HERE */

    // Main
    /** Create a new FloorCleaner object and call cleanFloor.   */
    public static void main(String[] arguments){
        FloorCleaner fc =new FloorCleaner();
        fc.cleanFloor();
    }        


}
