// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

import ecs100.*;
import java.awt.Color;
import java.util.*;
import java.awt.Point;

/** DodgemCar
 * Represents a single DodgemCar that can move around in the arena
 * 
 * A DodgemCar must have fields to store their state:
 *  - a position (x and y)
 *  - a directon of travel (an angle, preferable in radians, ie between -Math.PI and Math.PI
 *  - their remaining life
 *  - their color
 *
 * A DodgemCar needs
 *  - a constructor,
 *  - a method to make it move one unit
 *  - methods to turn the car to the left and the right
 *    A simple design changes the direction of the car directly.
 *    A better design has a "steering wheel": turning the car actually turns the
 *    steering wheel, and the steering wheel makes the direction change a little
 *    bit on each move.
 *  - a method to draw the car on the graphics pane
 *  - methods to check for collisions, either with the wall and obstacle, or with another car
 *    These methods should update the state of the car if there is a collision
 *  - a method to return the remaining life of the car.
 *
 * Drawing a DodgemCar should show the position and direction of the car.
 *   If the runing uses a steering wheel, then the current direction of the
 *   steering should also be shown.
 *   
 * A DodgemCar can collide with a wall, the obstacle, or another DodgemCar.
 * - Colliding with a wall gives a little bit of damage
 * - Colliding with the obstacle should give a lot of damage
 * - Colliding with the other car may give a little damage, but
 *   should also result in jumping back, and the directions of the cars being changed
 *  
 *   For a really nice design, the way the cars bump into each 
 *   other should depend on the relative directions of the cars -
 *   being bumped on the side should have a different effect from
 *   being bumped in the front or the back.
 *   
 */
public class DodgemCar{
    // Constants for the Geometry of the game.
    // (You may change or add to these if you wish)

    public static final int ArenaSize = 400;
    public static final int LeftWall = 30;
    public static final int RightWall = 30+ArenaSize;
    public static final int TopWall = 50;
    public static final int BotWall = TopWall+ArenaSize;

    public static final int ObstSize = 80;
    public static final int ObstRad = ObstSize/2;
    public static final int ObstX = LeftWall + ArenaSize/2;
    public static final int ObstY = TopWall + ArenaSize/2;
    
    
    // Constants: Geometry and other parameters
    /*# YOUR CODE HERE */
    private static final double turnrate = 0.314; 
    private static final double diameter = 25;
    // fields for the state of the car
    /*# YOUR CODE HERE */
    private Point position;
    private double direction;

    private double life = 100;
    private Color colour;
    //Constructor 
    /** 
     * The parameters specify the initial position and direction
     */
    public DodgemCar(double x, double y, double dir, Color Colour)  {
        /*# YOUR CODE HERE */
        this.position = new Point((int)x,(int)y);
        this.direction = dir;
        this.colour = Colour;
    }

    // other methods, eg for turning left & right, drawing, checking collisions, etc

    /**
     * Turn the steering wheel one step more to the left (negative angle)
     * This governs how much the car turns on each move
     * Steering wheel can't turn more than 45 degrees to left or right
     */
    public void turnLeft(){
        /*# YOUR CODE HERE */
        this.direction -= turnrate;
        this.direction %= Math.PI*2;
    }

    /**
     * Turn the steering wheel one step more to the right (positive angle)
     * This governs how much the car turns on each move
     * Steering wheel can't turn more than 45 degrees to left or right
     */
    public void turnRight(){
        /*# YOUR CODE HERE */
        this.direction += turnrate;
        this.direction %= Math.PI*2;
    }

    /**
     * Moves the car 1 unit forward
     * First changes the current direction according to the position of the
     * steering wheel (takes 20 moves to turn the car by the angle of the steering wheel)
     * Then moves forward by 1 unit
     */
    public void move() {
        //Vector conversion to x,y
        this.position.setLocation(this.position.getX() + Math.cos(this.direction),
                                  this.position.getY() + Math.sin(this.direction));
    }
    /** Same as move but backwards */
    public void moveBack() {
        //Vector conversion to x,y
        this.position.setLocation(this.position.getX() - Math.cos(this.direction),
                                  this.position.getY() - Math.sin(this.direction));
    }
    public void turnAround()
    {
        this.direction += Math.PI;
        this.direction %= Math.PI*2;
    }

    /** draw the car */
    public void draw() {
        /*# YOUR CODE HERE */
        UI.setColor(this.colour);
        UI.fillOval(this.position.getX(),this.position.getY(),
            diameter, diameter);
    }

    /**
     * Check whether the car has collided with a wall. (Core)
     * if so, move it back so it isn't overlapping the wall, and 
     * reduce its life by the appropriate amount.
     */
    public void checkCollideWall(){
        /*# YOUR CODE HERE */
        if (this.position.getX() < LeftWall)
        {
            //Left Wall
            this.moveBack();
            this.turnAround();
            this.life--;
        } else if (this.position.getX()+diameter > RightWall)
        {
            //Right Wall
            this.moveBack();
            this.turnAround();
            this.life--;
        }
                if (this.position.getY() < TopWall)
        {
            //Left Wall
            this.moveBack();
            this.turnAround();
            this.life--;
        } else if (this.position.getY()+diameter > ArenaSize)
        {
            //Right Wall
            this.moveBack();
            this.turnAround();
            this.life--;
        }
    }        

    /**
     * Check whether the car has collided with the obstacle. (Completion)
     * if so, move it back so it isn't overlapping the obstacle, and
     * reduce its life by the appropriate amount.
     */
    public void checkCollideObstacle(){
        /*# YOUR CODE HERE */
    }


    /** @return whether this car is touching the other car */
    public boolean checkCollideCar(DodgemCar other){
        /*# YOUR CODE HERE */
        if (this.position.distance(other.getPosition()) <= diameter )
        {
            return true;
        }
        return false;
    }
    public Point getPosition()
    {
        return this.position;
    }
    /**
     * Returns the amount of life left of this car (needed for Completion)
     */
    public double life(){
        /*# YOUR CODE HERE */
        return life;
    }
    public void hurt()
    {
        this.life--;
    }
    
    public double getDir()
    {return this.direction;}
    
    public void setDir(double angle)
    { this.direction = angle % Math.PI*2;}
    
    /**
     * Useful method for debugging: 
     * Returns a String rendering of the DodgemCar Object
     * which can be printed out for debugging.
     * Assumes that you called the fields x, y, and direction; you could change it.
     */
    public String toString(){
        return String.format("Car@(%.0f,%.0f)->%s", this.position.getX(), this.position.getY(), this.direction);
    }
}

