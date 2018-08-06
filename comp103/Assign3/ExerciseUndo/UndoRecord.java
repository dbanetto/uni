// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP103 Assignment
 */
import java.util.*;

/** UndoRecord: Stores information about an action that allows the
 *  ExerciseUndo program to undo a previous action.
 *  The actions are to move a card from a position in the hand onto one of
 *   discard piles. Undoing an action means taking a card off the appropriate
 *   discard pile and putting it back into the hand at the position it used to be.
 *  To do this, the program needs to know:
 *   the position in the hand that the card was taken from, and
 *   the pile the card was put on,
 *  Therefore, each UndoRecord will store a position in the hand, and a pile.
*/

public class UndoRecord{

    private int handPosition;
    private Stack<Card> pile;

    /** Construct a new UndoRecord object */
    public UndoRecord(int handPos, Stack<Card> discardPile){
	handPosition = handPos;
	pile = discardPile;
    }

    public int getHandPosition(){
	return handPosition;
    }

    public Stack<Card> getPile(){
	return pile;
    }

}
