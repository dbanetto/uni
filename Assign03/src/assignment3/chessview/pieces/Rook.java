package assignment3.chessview.pieces;

import assignment3.chessview.*;

public class Rook extends Piece {
	public Rook(boolean isWhite) {
		super(isWhite);
	}
	
	public boolean isValidMoveLookAhead(Position oldPosition, Position newPosition,
			Piece isTaken, Board board) {
		Piece p = board.pieceAt(oldPosition);
		Piece t = board.pieceAt(newPosition);

		int rowDelta = Math.abs(oldPosition.row() - newPosition.row());
		int colDelta = Math.abs(oldPosition.column() - newPosition.column());

		return ((rowDelta != 0 && colDelta == 0) || (rowDelta == 0 && colDelta != 0))
				&& (t == isTaken || (isTaken != null && isTaken.equals(t) && this.isWhite != isTaken.isWhite()))
				&& (board.clearColumnExcept(oldPosition, newPosition, p, t) ||
                    board.clearRowExcept(oldPosition, newPosition, p, t));
	}
	
	public String toString() {
		if(isWhite) {
			return "R";
		} else {
			return "r";
		}
	}
}
