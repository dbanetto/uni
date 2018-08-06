package cards.variations;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cards.core.*;
import cards.core.Player.Direction;
import cards.util.AbstractCardGame;

public class KnockOutWhist extends AbstractCardGame  implements Cloneable {
	private int hand = 13;

	public KnockOutWhist() {

	}

	public String getName() {
		return "Knock-Out Whist";
	}

	public boolean isGameFinished() {
		return hand == 0;
	}

	public void deal(List<Card> deck) {
		currentTrick = null;
		for (Player.Direction d : Player.Direction.values()) {
			players.get(d).getHand().clear();
		}
		Player.Direction d = Player.Direction.NORTH;
		for (int i = 0; i < hand * 4; ++i) {
			Card card = deck.get(i);
			players.get(d).getHand().add(card);
			d = d.next();
		}
	}

	public void endHand() {
		super.endHand();
		hand = hand - 1;
	}

	@Override
	public CardGame clone() {
		KnockOutWhist cardGame = new KnockOutWhist();

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
