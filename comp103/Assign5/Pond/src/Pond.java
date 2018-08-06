// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP103, Assignment 5
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;

import java.awt.Color;

/** Uses recursion to
- draw a path of white stepping stones across a pond.
- draw trees around the pond
 */
public class Pond implements UIButtonListener, UIMouseListener {

    // Fields
    private double lastX;
    private double lastY;

    public Pond() {
        UI.addButton("Clear", this);
        UI.addButton("Stepping Stones", this);
        UI.setMouseListener(this);
        drawPond();
        UI.printMessage("Click mouse to draw trees");
    }

    /** Draw a path of stepping stones, starting at (x,y)
     *	with the first stone of the given width, and each successive
     *	stone 80% of the previous stone.
     *	The stones look approximately right if the height of the oval
     *	is 1/4 the width.
     */
    public void steppingStones(double x, double y, double width) {
    	UI.fillOval(x, y, width, width * 0.25);
    	if (width * 0.8 > 4)
    		steppingStones(x + (width * 1.2) , y - (width * 0.4) , width * 0.8);
    }

    /** Draw a tree with the base at (xBot, yBot).
     *  The top of the first branch should be at xTop, yTop.
     *  Then draw three smaller trees on the top of this branch
     *   with tops above, to the left, and to the right of this branch.
     */
    public void drawTree(double xBot, double yBot, double xTop, double yTop) {
        
    	double dist = Math.sqrt( Math.pow(xTop - xBot, 2) + Math.pow(yTop - yBot, 2));
    	UI.drawLine(xTop, yTop, xBot, yBot);
    	double a = Math.atan2( (yBot - yTop), (xBot - xTop) );
    	
    	double l = (a + Math.PI/4);
    	double u = (a) ;
    	double r = (a - Math.PI/4);
    	
    	double prec = 0.5;
    	
    	UI.println("Dist : " + dist + " Angle : " + a);
    	UI.println("Left : " + l + " Center : " + u + " Right : " + r);
    	if (dist > 8) {
    		drawTree(xTop, yTop, xTop - (Math.cos(l) * (dist * prec)) , yTop - (Math.sin(l) * (dist * prec)));
    		drawTree(xTop, yTop, xTop - (Math.cos(u) * (dist * prec)) , yTop - (Math.sin(u) * (dist * prec)));
    		drawTree(xTop, yTop, xTop - (Math.cos(r) * (dist * prec)) , yTop - (Math.sin(r) * (dist * prec)));
    	}
    	
    }


    public void drawPond(){
        UI.clearGraphics();
        UI.setColor(Color.blue);
        UI.fillOval(50, 250, 400, 150);
    }

    /** Respond to button presses */
    public void buttonPerformed(String button) {
        if ( button.equals("Clear")) {
            drawPond();

        }
        else if (button.equals("Stepping Stones")) {
            UI.setColor(Color.gray);
            steppingStones(100, 350, 40);
        }
    }

    /** Respond to mouse events */
    public void mousePerformed(String action, double x, double y) {
        if (action.equals("released")) {
            UI.setColor(Color.green.darker().darker());
            drawTree(x, y, x, y-50);
        }
    }

    // Main
    public static void main(String[] arguments){
        new Pond();
    }	

}
