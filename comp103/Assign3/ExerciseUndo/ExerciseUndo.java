// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP103 Assignment 3
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.util.*;

import java.awt.Color;
import java.io.*;


/** ExerciseUndo   */

public class ExerciseUndo implements UIButtonListener, UIMouseListener{

    // Constants for the positions of the deck, the hand, and the discard pile
    public static final int HAND_X = 20;
    public static final int HAND_Y = 200;
    public static final int HAND_SEP = 15;

    public static final int DISCARD1_X = 20;
    public static final int DISCARD2_X = 150;
    public static final int DISCARD_Y = 12;

    // constants for drawing the board
    public static final Color FELT_COLOR = new Color(32, 112, 48);
    public static final int TABLE_WIDTH = 900;
    public static final int TABLE_HEIGHT = 500;
    public static final int CARD_WIDTH = 75;
    public static final int CARD_HEIGHT = 110;

    private List<Card> hand;                 // the list of cards in the hand
    private Stack <Card> pile1 = new Stack<Card>();  // the two discard piles.
    private Stack <Card> pile2 = new Stack<Card>();  //
    
    // the index of the currently selected (or -1)
    private int selected = -1;

    /** Undo stack records the position in the hand that the card was discarded from.
     */
    private Stack<UndoRecord> undoStack = new Stack<UndoRecord>();

    /**
     *   Constructor: set up interface and make the new hand.
     */
    public  ExerciseUndo(){
	UI.addButton("Sorted Hand", this);
	UI.addButton("Shuffled Hand", this);
	UI.addButton("Undo", this);
        UI.setMouseListener(this);
        //initialise the hand.
	newHand();
        Collections.shuffle(hand);
        redraw();
    }

    public void buttonPerformed(String button){
	if (button.equals("Sorted Hand")){
	    newHand();
	}
	if (button.equals("Shuffled Hand")){
	    newHand();
	    Collections.shuffle(hand);
	}
	if (button.equals("Undo")){
	    undo();
	}
	redraw();
    }


    /**
     * Respond to the mouse:
     * If the mouse position is on a card in the hand:
     * it moves this card to the discard pile
     */
    public void mousePerformed(String action, double x, double y) {
        if (action.equals("released")) {
	    // clicking on a card in the hand selects it.
            for (int i = hand.size() - 1; i >= 0; i--) {
		if (pointAtPlace(x, y, HAND_X+i*HAND_SEP, HAND_Y)) {
		    selected = i;
		    redraw();
                    return;
                }
            }
	    // If there is a selected card, clicking on a discard pile
	    // moves the card out of the hand and onto the discard pile.
	    // It should record the action on the undo stack so that the
	    // move could be undone later.
	    if (selected > -1){
		if (pointAtPlace(x, y, DISCARD1_X, DISCARD_Y)) {
		    Card card = hand.remove(selected);
		    pile1.push(card);
		    /*# YOUR CODE HERE */
		    selected = -1;
		    redraw();
                }
		else if (pointAtPlace(x, y, DISCARD2_X, DISCARD_Y)) {
		    Card card = hand.remove(selected);
		    pile2.push(card);
		    /*# YOUR CODE HERE */
		    selected = -1;
		    redraw();
		}
	    }
	}
    }

    /** Undo another action.
     *  The only actions are putting a card from some position in the hand
     *  on to one of the discard piles.  Undoing an action means putting the card 
     *  from the discard pile back into the hand at the position is was in.
     *  The undo stack records the position in the hand that a card was taken from
     *  and the pile it was put on.
     *  At each undo step, the program should retrieve the top record on the
     *  undo stack, and then take the card from the top of the specified discard pile
     *  and put it into the hand at the specified position.
     */

    public void undo(){
	/*# YOUR CODE HERE */
    }



    /** Create a new hand, empty the discard piles, empty the undo stack.*/
    public void newHand(){
	hand = new ArrayList<Card>();
	undoStack.clear();
	pile1.clear();
	pile2.clear();
        for (String suit : new String[]{"spades", "diamonds", "clubs", "hearts"}){
	    for (int rank = 1; rank <= 13; rank++) {
		Card card = new Card(suit, rank);
		hand.add(new Card(suit, rank));
	    }
	}
    }



    /** would the point (x,y) be on top of a card placed at (cardX, cardY) */
    public boolean pointAtPlace(double x, double y, double cardX, double cardY){
	return (x>=cardX && y >=cardY  &&
		x <= cardX+CARD_WIDTH &&
		y <= cardY+CARD_HEIGHT);
    }


    /**
     Draw the table: the background, the hand, and the discard piles.
     */

    public void redraw() {
        // draw background
        UI.clearGraphics(false);
        UI.setColor(FELT_COLOR);
        UI.fillRect(0, 0, TABLE_WIDTH, TABLE_HEIGHT, false);

        // draw hand
        int x = HAND_X;
        for (int i = 0; i < hand.size(); i++) {
	    if (i==selected){
                hand.get(i).draw(x, HAND_Y - HAND_SEP);
            }
	    else {
		hand.get(i).draw(x, HAND_Y);
	    }
            x += HAND_SEP;
        }

        // draw discard piles
	this.drawPile(pile1, DISCARD1_X, DISCARD_Y);
	this.drawPile(pile2, DISCARD2_X, DISCARD_Y);
	UI.repaintGraphics();
    }


    public void drawPile(Stack<Card> pile, int x, int y){
	if (pile.isEmpty()){
	    UI.setColor(Color.lightGray);
	    UI.fillRect(x, y, CARD_WIDTH, CARD_HEIGHT, false);
	}
	else {
	    pile.peek().draw(x, y);
	}
    }

    public static void main(String[] arguments){
       ExerciseUndo obj = new ExerciseUndo();
    }	


}
