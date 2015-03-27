package assignment3.chessview.pieces;

import assignment3.chessview.*;



public abstract class Piece {
	private boolean moved = false;
	private Position lastPosition = null;

	protected boolean isWhite;

	public Piece(boolean isWhite) {
		this.isWhite = isWhite;
	}

	/**
	 * Determine whether this piece is white or black.
	 * @return if the Piece is white returns true, otherwise false
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
	 * @return True if this piece has a valid move from old to new position, otherwise false
	 */
	public boolean isValidMove(Position oldPosition,
			Position newPosition, Piece isTaken, Board board) {
		Piece p = board.pieceAt(oldPosition);
		return this.isValidMoveLookAhead(oldPosition, newPosition, isTaken, board) && this.equals(p);
	}

	/**
	 * Check whether or not a given move on a given board is valid.
	 * Ignores current piece
	 *
	 * @param oldPosition
	 *            --- position of this piece before move.
	 * @param newPosition
	 *            --- position of this piece after move.
	 * @param isTaken
	 *            --- piece being taken, or null if no piece taken.
	 * @param board
	 *            --- board on which the validity of this move is being checked.
	 * @return True if this type of piece would have a valid move between old and new position, otherwise false
	 */
	public abstract boolean isValidMoveLookAhead(Position oldPosition,
										Position newPosition, Piece isTaken, Board board);

	/**
	 * Check if the Piece has moved
	 * @return True if the piece has moved before, else False
	 */
	public boolean hasMoved() { return this.moved; }

	/**
	 * Get the Piece's last position
	 * @return A Position if the piece has moved before, otherwise null
	 */
	public Position getLastPosition() {
		return lastPosition;
	}

	/**
	 * Update the Piece moved fields to keep track of piece movements
	 * @param oldPosition the location where the piece was BEFORE the movement
	 */
	public void Moved(Position oldPosition) {
		this.moved = true;
		this.lastPosition = oldPosition;
	}
}
