package assignment3.chessview.pieces;

import java.util.Arrays;

import assignment3.chessview.*;



public abstract class Piece {
	private boolean moved = false;

	protected boolean isWhite;

	public Piece(boolean isWhite) {
		this.isWhite = isWhite;
	}

	/**
	 * Determine whether this piece is white or black.
	 * @return
	 */
	public boolean isWhite() {
		return isWhite;
	}

	public boolean equals(Object o) {
		if (o instanceof Piece) {
			Piece p = (Piece) o;
			return o.getClass() == getClass() && isWhite == p.isWhite;
		}
		return false;
	}

	/**
	 * Check whether or not a given move on a given board is valid. For takes,
	 * the piece being taken must be supplied.
	 * 
	 * @param oldPosition
	 *            --- position of this piece before move.
	 * @param newPosition
	 *            --- position of this piece after move.
	 * @param isTaken
	 *            --- piece being taken, or null if no piece taken.
	 * @param board
	 *            --- board on which the validity of this move is being checked.
	 * @return
	 */
	public boolean isValidMove(Position oldPosition,
			Position newPosition, Piece isTaken, Board board) {
		Piece p = board.pieceAt(oldPosition);
		return this.isValidMoveLookAhead(oldPosition, newPosition, isTaken, board) && this.equals(p);
	}

	public abstract boolean isValidMoveLookAhead(Position oldPosition,
										Position newPosition, Piece isTaken, Board board);

	public boolean hasMoved() { return this.moved; }
	public void Moved() { this.moved = true; }
}
