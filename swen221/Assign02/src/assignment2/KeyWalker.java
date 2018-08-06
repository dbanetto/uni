package assignment2;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import static maze.Direction.*;
import maze.*;
import maze.gui.MazeWindow;

/**
 * An any implementation of the key walker, which you need to complete
 * following the notes.
 * 
 */
public class KeyWalker extends Walker {
	
	private Direction toMove;
    private boolean addedListener = false;

	public KeyWalker() {
		super("Key Walker");

	}

	protected Direction move(View v) {
        if (!addedListener) {
            // Add Key Listener for arrows, WASD and hjkl input formats
            MazeWindow.mainWindow.addKeyListener(new KeyListener() {
                @Override
                public void keyTyped(KeyEvent e) {}

                @Override
                public void keyPressed(KeyEvent e) {}

                @Override
                public void keyReleased(KeyEvent e) {
                        switch (e.getKeyCode()) {
                            case (KeyEvent.VK_W):
                            case (KeyEvent.VK_J):
                            case (KeyEvent.VK_UP):
                                toMove = Direction.NORTH;
                                MazeWindow.mainWindow.repaint();
                                break;
                            case (KeyEvent.VK_S):
                            case (KeyEvent.VK_K):
                            case (KeyEvent.VK_DOWN):
                                toMove = Direction.SOUTH;
                                MazeWindow.mainWindow.repaint();
                                break;
                            case (KeyEvent.VK_A):
                            case (KeyEvent.VK_H):
                            case (KeyEvent.VK_LEFT):
                                toMove = Direction.WEST;
                                MazeWindow.mainWindow.repaint();
                                break;

                            case (KeyEvent.VK_D):
                            case (KeyEvent.VK_L):
                            case (KeyEvent.VK_RIGHT):
                                toMove = Direction.EAST;
                                MazeWindow.mainWindow.repaint();
                                break;
                        }

                }
            });
            addedListener = true;
        }

        Direction toReturn = toMove;
        toMove = null;
		return toReturn;
	}
}