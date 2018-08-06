package assignment3.chessview.pieces;

import assignment3.chessview.*;

public class Bishop extends Piece {
	public Bishop(boolean isWhite) {
		super(isWhite);
	}

	/**
	 *  @see super.isValidMoveLookAhead
	 */
	public boolean isValidMoveLookAhead(Position oldPosition, Position newPosition, Piece isTaken, Board board) {
		Piece p = board.pieceAt(oldPosition);
		Piece t = board.pieceAt(newPosition);

		// based abs values
		int rowDelta = Math.abs(oldPosition.row() - newPosition.row());
		int colDelta = Math.abs(oldPosition.column() - newPosition.column());
		if (colDelta != rowDelta) {
			return false;
		}

		return  (t == isTaken || (isTaken != null && isTaken.equals(t) && this.isWhite != isTaken.isWhite()))
				&& (board.clearDiagonalExcept(oldPosition, newPosition, p, t));
	}

	public String toString() {
		if(isWhite) {
			return "B";
		} else {
			return "b";
		}
	}
}
