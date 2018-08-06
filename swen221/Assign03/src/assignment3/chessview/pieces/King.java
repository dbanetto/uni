package assignment3.chessview.pieces;

import assignment3.chessview.*;

public class King extends Piece {
	public King(boolean isWhite) {
		super(isWhite);
	}

	/**
	 *  @see super.isValidMoveLookAhead
	 */
	public boolean isValidMoveLookAhead(Position oldPosition, Position newPosition,
			Piece isTaken, Board board) {

		int diffCol = Math.abs(oldPosition.column() - newPosition.column());
		int diffRow = Math.abs(oldPosition.row() - newPosition.row());

		Piece p = board.pieceAt(oldPosition);
		Piece t = board.pieceAt(newPosition);


		return  (t == isTaken || (isTaken != null && isTaken.equals(t) && this.isWhite != isTaken.isWhite()))
				&& (diffCol == 1 || diffRow == 1) && diffCol <= 1
				&& diffRow <= 1;
	}
	
	public String toString() {
		if(isWhite) {
			return "K";
		} else {
			return "k";
		}
	}
}
