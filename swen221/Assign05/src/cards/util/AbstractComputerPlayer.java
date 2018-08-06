package cards.util;

import cards.core.*;

/**
 * Represents an computer player in the game. This class can be overriden with
 * different implementations that use different kinds of A.I. to determine
 * appropriate moves.
 *
 * @author David J. Pearce
 *
 */
public abstract class AbstractComputerPlayer implements Cloneable {
	protected Player player;

    public AbstractComputerPlayer(Player player) {
    	this.player = player;
    }

	abstract public Card getNextCard(Trick trick);

	public void setPlayer(Player player) {
		this.player = player;
	}

	public AbstractComputerPlayer clone() {
		try {
			AbstractComputerPlayer ap = (AbstractComputerPlayer)super.clone();
			ap.player = this.player.clone();
			return ap;
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}
}
