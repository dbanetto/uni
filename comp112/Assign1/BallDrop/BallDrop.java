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
 * BallDrop is a simple program that lets you drop balls from the top of the window
 * The constructor sets up two buttons and draws the "ground"
 * There is a method to respond to the buttons, and
 * a method that drops a ball, using a loop that repeatedly
 * erases the ball, moves the position of the ball, and redraws it
 * until it gets to the ground.
 */

public class BallDrop implements UIButtonListener{

    public static final double size = 20;  // the size of the ball that gets dropped.
    public static final double ground = 400;  // the size of the ball that gets dropped.

    /**
     * Construct a new BallDrop object, setting up the buttons.
     */
    public BallDrop(){
        UI.initialise();
        UI.addButton("Drop", this);
        UI.addButton("Quit", this);
        UI.drawLine(20, ground, 600, ground);
    }

    /**
     * Respond to the buttons
     * The drop button will choose a random position
     * and drop a ball from there.
     */
    public void buttonPerformed(String button){
        if (button.equals("Drop")){
            double dropPosition = 50 + Math.random()*500;
            this.doDrop(dropPosition);
        }
        if (button.equals("Quit")){
            UI.quit();
        }
    }

    /**
     * Drop a ball from the top of the graphics pane at position x.
     */
    public void doDrop(double x){
        double y = 0;
        UI.setColor(Color.red);
        while (y < ground-size){
            UI.eraseOval(x, y, size, size);
            y = y + 1;
            UI.fillOval(x, y, size, size);
            UI.sleep(15);
        }
    }

    /**
     * Main method that is called on the class, rather than on an object
     */
    public static void main(String[] arguments){
        BallDrop obj = new BallDrop();
    }        

}
