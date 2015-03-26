package assignment3.chessview.pieces;

import assignment3.chessview.*;

public class Knight extends Piece {
	public Knight(boolean isWhite) {
		super(isWhite);
	}
		
	public boolean isValidMoveLookAhead(Position oldPosition, Position newPosition,
			Piece isTaken, Board board) {
		int diffCol = Math.max(oldPosition.column(), newPosition.column())
				- Math.min(oldPosition.column(), newPosition.column());
		int diffRow = Math.max(oldPosition.row(), newPosition.row())
				- Math.min(oldPosition.row(), newPosition.row());
		Piece p = board.pieceAt(oldPosition);
		Piece t = board.pieceAt(newPosition);

		if (!((diffCol == 2 && diffRow == 1) || (diffCol == 1 && diffRow == 2))) {
			return false;
		}

		return (t == isTaken || (isTaken != null && isTaken.equals(t) && this.isWhite != isTaken.isWhite()))
				&& ((diffCol == 2 && diffRow == 1) || (diffCol == 1 && diffRow == 2));
	}
	
	public String toString() {
		if(isWhite) {
			return "N";
		} else {
			return "n";
		}
	}
}
