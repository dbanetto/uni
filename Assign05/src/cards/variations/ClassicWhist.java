package cards.variations;

import java.util.List;

import cards.core.Card;
import cards.core.CardGame;
import cards.core.Player;
import cards.util.AbstractCardGame;

/**
 * A simple variation of Whist where only a single hand is played.
 *
 * @author David J. Pearce
 *
 */
public class ClassicWhist extends AbstractCardGame implements Cloneable {

	public ClassicWhist() {

	}

	public String getName() {
		return "Single Hand Whist";
	}

	public boolean isGameFinished() {
		for (Player.Direction d : Player.Direction.values()) {
			if (scores.get(d) == 5) {
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

	public ClassicWhist clone() {
		ClassicWhist cardGame = new ClassicWhist();

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
