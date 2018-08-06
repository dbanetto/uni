package cards.core;

import java.util.Comparator;

public class Card implements Comparable<Card> {

	/**
	 * Represents a card suit.
	 *
	 * @author David J. Pearce
	 *
	 */
	public enum Suit {
		HEARTS,
		CLUBS,
		DIAMONDS,
		SPADES,
	}

	/**
	 * Represents the different card "numbers".
	 *
	 * @author David J. Pearce
	 *
	 */
	public enum Rank {
		TWO,
		THREE,
		FOUR,
		FIVE,
		SIX,
		SEVEN,
		EIGHT,
		NINE,
		TEN,
		JACK,
		QUEEN,
		KING,
		ACE,
	}

	// =======================================================
	// Card stuff
	// =======================================================

	private Suit suit; // HEARTS, CLUBS, DIAMONDS, SPADES
	private Rank rank; // 2 <= number <= 14 (ACE)

	/**
	 * Construct a card in the given suit, with a given number
	 *
	 * @param suit
	 *            --- between 0 (HEARTS) and 3 (SPADES)
	 * @param number
	 *            --- between 2 and 14 (ACE)
	 */
	public Card(Suit suit, Rank number) {
		this.suit = suit;
		this.rank = number;
	}

	/**
	 * Get the suit of this card, between 0 (HEARTS) and 3 (SPADES).
	 *
	 * @return
	 */
	public Suit suit() {
		return suit;
	}

	/**
	 * Get the number of this card, between 2 and 14 (ACE).
	 *
	 * @return
	 */
	public Rank rank() {
		return rank;
	}

	private static String[] suits = { "Hearts","Clubs","Diamonds","Spades"};
	private static String[] ranks = { "2 of ", "3 of ", "4 of ",
			"5 of ", "6 of ", "7 of ", "8 of ", "9 of ", "10 of ", "Jack of ",
			"Queen of ", "King of ", "Ace of " };

	public String toString() {
		return ranks[rank.ordinal()] + suits[suit.ordinal()];
	}

	@Override
	public int compareTo(Card o) {
		int result  = compareSuites(this.suit, o.suit);
		if (result == 0) {
			result = compareRank(this.rank, o.rank);
		}
		return result;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		Card card = (Card) o;

		if (suit != card.suit) return false;
		return rank == card.rank;

	}

	@Override
	public int hashCode() {
		int result = suit != null ? suit.hashCode() : 0;
		result = 31 * result + (rank != null ? rank.hashCode() : 0);
		return result;
	}

	/**
	 * Compare two suits
	 *
	 * @param suit1
	 * @param suit2
	 * @return zero if equal, positive if suit1 is greater and negative if suit2 is greater
	 */
	public static int compareSuites(Suit suit1, Suit suit2) {
		int s1 = 0, s2 = 0, i = 0;
		// relies on the fact that the enum is entered in the right order
		for (Suit s : Suit.values()) {
			i++;
			if (suit1 == s) {
				s1 = i;
			}
			if (suit2 == s) {
				s2 = i;
			}
		}
		return s1 - s2;
	}

	/**
	 * Compare two ranks
	 *
	 * @param rank1
	 * @param rank2
	 * @return zero if equal, positive if rank1 is greater and negative if rank2 is greater
	 */
	public static int compareRank(Rank rank1, Rank rank2) {
		int s1 = 0, s2 = 0, i = 0;
		// relies on the fact that the enum is entered in the right order
		for (Rank s : Rank.values()) {
			i++;
			if (rank1 == s) {
				s1 = i;
			}
			if (rank2 == s) {
				s2 = i;
			}
		}
		return s1 - s2;
	}

	/**
	 * Compares two cards with cards of the suit equal to trumps are given
	 * highest order
	 *
	 * @param card1
	 * @param card2
	 * @param trumps
	 * @return zero if equal, positive if rank1 is greater and negative if rank2 is greater
	 */
	public static int compareTrumps(Card card1, Card card2, Suit trumps) {
		if (card1 == card2) { return 0; }
		if (card1 == null) { return -1; }
		if (card2 == null) { return  1; }

		if (card1.suit() == trumps && card1.suit() == trumps) {
			return Card.compareRank(card1.rank(), card2.rank());
		} else if (card1.suit() == trumps && card2.suit() != trumps) {
			return 1;
		} else if (card1.suit() == trumps && card2.suit() != trumps) {
			return -1;
		} else {
			return card1.compareTo(card2);
		}
	}

	public Card clone() {
		return new Card(this.suit, this.rank);
	}

}
