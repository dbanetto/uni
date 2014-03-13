// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment 1  
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
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
    
    private double x = 0, y = 0;
    private double dir = 0.0;
    private final double radius = 20;
    private final int width = 600 , height = 480;
    
    
    /*# YOUR CODE HERE */
    public void cleanFloor()
    {
        UI.setImmediateRepaint(false);
        
        UI.setColor(Color.GRAY);
        UI.clearGraphics(false);
        UI.fillRect(0,0,width, height);
        
        UI.setColor(Color.RED);
        x = 100;
        y = 240;
        dir = -Math.PI / 2;
        
        while (true)
        {
            //Remove ball from last frame
            UI.eraseOval(x, y, radius*2, radius*2);
            //move ball
            move();
            //Draw in the new place
            
            
            //Check if the ball is at the edge
            //Left collision
            if (x < 0 )
            {
                //Change direction
                chanageDir(0);
                
            } 
            //Right Collision
            else if (x + radius*2 > width)
            {
                //Change direction
                chanageDir(1);
            }
            //Top collision
            if (y < 0)
            {
                //Change Direction
                chanageDir(2);
                
            } 
            //Bottom Collision
            else if (y + radius*2 > height)
            {
                //Change direction
                chanageDir(3);
            }
        
            //Find the x,y co-ordients of the line to show direction of ball
            int x_dot = 0, y_dot = 0;
            x_dot =(int)Math.round( (Math.cos(this.dir) * radius-1)); //- (Math.sin(this.dir) * radius-5) );
            y_dot =(int)Math.round( (Math.sin(this.dir) * radius-1)); // + (Math.cos(this.dir) * radius-5) );
            
            //Render main ball
            UI.setColor(Color.red);
            UI.fillOval(x, y, radius*2, radius*2);
            
            //Render directional line
            UI.setColor(Color.green);
            UI.drawLine(x+radius,         y+radius,
                        x+radius + x_dot, y+radius + y_dot);
            //PAINT!
            UI.repaintGraphics();
            UI.sleep(15);
        }
    }
    
    private void move()
    {
        this.dir = this.dir % (Math.PI*2);
        this.x += Math.cos(this.dir) * 1;
        this.y += Math.sin(this.dir) * 1;
    }
    
    private void chanageDir(int side)
    {
        switch (side)
        {
           
            //Towards left side
            case(0):
                this.dir = Math.random() * Math.PI - Math.PI/2;
                break;
           
            //Towards right side
            case(1):
                this.dir = Math.random() * Math.PI + Math.PI/2;
                break;
            
            //Towards top
            case (2):
                this.dir = Math.random() * Math.PI;
                break;
            
            //Towards bottom
            case(3):
                this.dir = Math.random() * -Math.PI;
                break;
        }
    }
    
    // Main
    /** Create a new FloorCleaner object and call cleanFloor.   */
    public static void main(String[] arguments){
        UI.initialise();
        FloorCleaner fc =new FloorCleaner();
        fc.cleanFloor();
    }        


}
