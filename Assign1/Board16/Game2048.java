// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP 103 Assignment 1
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;

public class Game2048 implements UIButtonListener, UIKeyListener {

    Board2048 game = null;

    private final int SIZE = 5;
    private boolean hasReachedTarget = false;

    public Game2048 () {
    	UI.initialise();
    	UI.setImmediateRepaint(false);
    	
        UI.addButton("Restart", this);
        UI.addButton("Left", this);
        UI.addButton("Right", this);
        UI.addButton("Up", this);
        UI.addButton("Down", this);

        UI.setKeyListener(this);

        UI.println("Move the tiles with the arrow keys but click on the graphics pane first.");
        UI.println("or use the Left and Right buttons");
        UI.println("Each time 2 tiles with the same number touch, the numbers are added and the two tiles merge.");
        UI.println("Produce the magic number of 16.");
        UI.println("");
        UI.println("NOTE : WASD and HJKL is supported!");

        startGame();
    }

    /** Respond to button presses */
    public void buttonPerformed(String button) {
        if (button.equals("Restart"))
            startGame();
        else 
            move (button);
    }

    /** Respond to key actions */
    public void keyPerformed(String key) {
        move(key);
    }

    private void startGame() {
        game = new Board2048 (SIZE);
        game.insertRandomTile();
        game.redraw();
    }

    private void move(String direction) {
        if (game == null) {
            UI.println("game need to be restarted");
            return;
        }

        if (direction.equals("Left") || direction.equals("a") || direction.equals("h"))
            game.move(Direction.LEFT);
        else if (direction.equals("Right") || direction.equals("d") || direction.equals("l"))
        	game.move(Direction.RIGHT);
        else if (direction.equals("Up") || direction.equals("w") || direction.equals("k"))
        	game.move(Direction.UP);
        else if (direction.equals("Down") || direction.equals("s") || direction.equals("j"))
        	game.move(Direction.DOWN);
        game.redraw();

        // Only display the message the first time
        if (! hasReachedTarget && game.hasReachedTarget()) {
            hasReachedTarget = true;
            UI.println("Game won!!!");
            UI.println("you can restart a new game or carry this game");
        }

        // Insert a new random tile
        UI.sleep(20);
        game.insertRandomTile();
        game.redraw();

        // Check if game is over
        if (game.isGameOver()) {
            UI.println("Game OVER!!!");
            game = null;
        }
    }

    public static void main(String[] arguments){
        new Game2048();
    }   
}
