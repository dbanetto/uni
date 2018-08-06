package cards.util;

import cards.core.Card;
import cards.core.Player;
import cards.core.Trick;

import java.util.*;

/**
 * Implements a simple computer player who plays the highest card available when
 * the trick can still be won, otherwise discards the lowest card available. In
 * the special case that the player must win the trick (i.e. this is the last
 * card in the trick), then the player conservatively plays the least card
 * needed to win.
 *
 * @author David J. Pearce
 *
 */
public class SimpleComputerPlayer extends AbstractComputerPlayer {

	public SimpleComputerPlayer(Player player) {
		super(player);
	}


	@Override
	public Card getNextCard(Trick trick) {
		Card.Suit trumps = trick.getTrumps();

        // First to play, play best
		if (trick.getCardsPlayed().isEmpty()) {
			return bestCard(trick, trumps);
		} else {
            // Best played
			Card trickBest = bestCardInTrick(trick, trumps);
            // Our best
			Card handBest = bestCard(trick, trumps);
            // Can we beat them?
			if (compareCards(handBest, trickBest, trumps) < 0 || Card.compareTrumps(handBest, trickBest, trumps) < 0) {
                // No, dump!
				return worstCard(trick, trumps);
			} else {
                // We are last to play, time to min-max cards
				if (trick.getCardsPlayed().size() == 3) {
					return marginalWinCard(trick, trickBest, trumps);
				}
                // Play best and hope to win
				return handBest;

			}
		}
	}

    /**
     * Get the best eligible card from the players hand
     *
     * @param trick the current trick being played
     * @param trumps the trump for this round
     * @return Best card eligible from the players hand
     */
	private Card bestCard(Trick trick, Card.Suit trumps) {
		List<Card> eligible = getEligible(trick);
		Card best = eligible.get(0);
		for (Card c : eligible) {
			if (compareCards(best, c, trumps) < 0) {
				best = c;
			}
		}

		return best;
	}

    /**
     * Get the worst eligible card from the players hand
     *
     * @param trick the current trick being played
     * @param trumps the trump for this round
     * @return Worst card eligible from the players hand
     */
	private Card worstCard(Trick trick, Card.Suit trumps) {
		List<Card> eligible = getEligible(trick);
		Card worst = eligible.get(0);
		for (Card c : eligible) {
			if (compareCards(worst, c, trumps) > 0) {
				worst = c;
			}
		}
		return worst;
	}

    /**
     * Get the best card played in the trick
     * @param trick the current trick being played
     * @param trumps the trump for this round
     * @return Best card played in the trick
     */
	private Card bestCardInTrick(Trick trick, Card.Suit trumps) {
		Card best = null;
		for (Card c : trick.getCardsPlayed()) {
			if (compareCards(best, c, trumps) < 0) {
				best = c;
			}
		}
		return best;
	}

    /**
     * Get the lowest card needed to win
     *
     * @param trick the current trick being played
     * @param trickBest best card in trick
     * @param trumps the trump for this round
     * @return The lowest valued eligible card needed to win the trick
     */
	private Card marginalWinCard(Trick trick, Card trickBest, Card.Suit trumps) {
		List<Card> eligible = getEligible(trick);
		List<Card> beats = new ArrayList<>();
        // Get a list of cards that can beat trickBest
		for (Card c : eligible) {
			if (Card.compareTrumps(c, trickBest, trumps) > 0) {
				beats.add(c);
			}
		}

        // Get the lowest card that can beat trickBest
		Card selected = beats.get(0);
		for (Card c : beats) {
			if (selected.compareTo(c) > 0) {
				selected = c;
			}
		}
		return selected;
	}

    /**
     * Compare two cards based on the "highest eligible"
     *
     * the highest eligible card is then the card with the highest rank and suit
     * where the current suit of trumps (if applicable) is always the highest
     * suit. In the case of two equally ranked
     * cards of non-trump suit, then the underlying ordering implied by
     * Card.compareTo() should be used to chose.
     * @param o1 Card 1
     * @param o2 Card 2
     * @param trumps the trump for this round
     * @return Positive if o1 is greater than o2, negative if o2 is greater than o1 and 0 if equal
     */
	private int compareCards(Card o1, Card o2, Card.Suit trumps) {
		if (o1 == o2) { return 0; }
		if (o1 == null) { return -1; }
		if (o2 == null) { return  1; }

		if (o1.suit() == trumps && o2.suit() == trumps) {
			return Card.compareRank(o1.rank(), o2.rank());
		} else if (o1.suit() == trumps && o2.suit() != trumps) {
			return 1;
		} else if (o1.suit() == trumps && o2.suit() != trumps) {
			return -1;
		} else {
			if (o1.rank() == o2.rank()) {
				return o1.compareTo(o2);
			} else {
				return Card.compareRank(o1.rank(), o2.rank());
			}
		}
	}

    /**
     * Get a list of cards eligible for play
     *
     * @param trick the current trick being played
     * @return a list of cards eligible for play
     */
	private List<Card> getEligible(Trick trick) {
		List<Card> eligible = new ArrayList<>();
		Card firstCard = (trick.getCardsPlayed().isEmpty() ? null : trick.getCardsPlayed().get(0));

		boolean hasSuit =  (firstCard != null && !this.player.getHand().matches(firstCard.suit()).isEmpty());
		if (hasSuit) {
            // has suit, follow it
			for (Card c : player.getHand()) {
				if (c.suit().equals(firstCard.suit())) {
					eligible.add(c);
				}
			}
		} else {
            // No suit or no cards with suit
			for (Card c : player.getHand()) {
				eligible.add(c);
			}
		}

		return eligible;
	}

}
