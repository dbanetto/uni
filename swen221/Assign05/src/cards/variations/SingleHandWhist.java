package cards.variations;

import java.util.List;
import java.util.Set;

import cards.core.*;
import cards.core.Player.Direction;
import cards.util.AbstractCardGame;

/**
 * An implementation of the "classical" rules of Whist.
 *
 * @author David J. Pearce
 *
 */
public class SingleHandWhist extends AbstractCardGame implements Cloneable {

	public SingleHandWhist() {

	}

	public String getName() {
		return "Classic Whist";
	}

	public boolean isGameFinished() {
		for (Player.Direction d : Player.Direction.values()) {
			if (scores.get(d) == 1) {
				return true;
			}
		}
		return false;
	}

	public void deal(List<Card> deck) {
		currentTrick = null;
		for (Player.Direction d : Player.Direction.values()) {
			players.get(d).getHand().clear();
		}
		Player.Direction d = Player.Direction.NORTH;
		for (int i = 0; i < deck.size(); ++i) {
			Card card = deck.get(i);
			players.get(d).getHand().add(card);
			d = d.next();
		}
	}

	@Override
	public AbstractCardGame clone() {
		SingleHandWhist cardGame = new SingleHandWhist();

		for (Player.Direction d : this.players.keySet()) {
			cardGame.players.put(d, this.players.get(d).clone());
		}
		for (Player.Direction d : this.tricks.keySet()) {
			cardGame.tricks.put(d, new Integer(this.tricks.get(d)));
		}

		for (Player.Direction d : this.scores.keySet()) {
			cardGame.scores.put(d, new Integer(this.scores.get(d)));
		}

		cardGame.trumps = this.trumps;
		cardGame.currentTrick = (this.currentTrick != null ? this.currentTrick.clone() : null);

		return cardGame;
	}
}
