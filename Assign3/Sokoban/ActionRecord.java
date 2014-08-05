// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP 103 Assignment 3 */

/** A class to record an action:  move or push in a given direction.
 */

public class ActionRecord {
    private String kind;
    private String dir;
    
    public ActionRecord(String a, String d) {
	if (a.equalsIgnoreCase("push"))
	    kind = "push";
	else 
	    kind = "move";
	dir = d;
    }
      
    public boolean isMove() {
	return kind.equalsIgnoreCase("move");
    }

    public boolean isPush() {
	return kind.equalsIgnoreCase("push");
    }
    
    public String dir() {
	return dir;
    }
    
    public String toString() {
	return (kind + " to " + dir);
    }

    /** Test method */
    public static void main(String[] args) {
	ActionRecord A = new ActionRecord("push", "up");
	System.out.println(A + " is a push: " + A.isPush());
	System.out.println(A + " is a Move: " + A.isMove());
	ActionRecord B = new ActionRecord("move", "right");
	System.out.println(B + " is a push: " + B.isPush());
	System.out.println(B + " is a Move: " + B.isMove());
    }
}
