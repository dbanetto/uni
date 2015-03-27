package assignment3.chessview.pieces;

import assignment3.chessview.*;

public class Queen extends Piece {
	public Queen(boolean isWhite) {
		super(isWhite);
	}
	
	public boolean isValidMoveLookAhead(Position oldPosition, Position newPosition,
			Piece isTaken, Board board) {
		Piece p = board.pieceAt(oldPosition);
		Piece t = board.pieceAt(newPosition);

		int deltaRow = Math.abs(oldPosition.row() - newPosition.row());
		int deltaCol = Math.abs(oldPosition.column() - newPosition.column());



		return  (t == isTaken || (isTaken != null && isTaken.equals(t) && this.isWhite != isTaken.isWhite()))
				&& ((deltaCol == deltaRow && deltaCol != 0) || (deltaCol != 0 && deltaRow == 0) || (deltaRow != 0 && deltaCol == 0))
				&& (board.clearColumnExcept(oldPosition, newPosition, p,t)
                    || board.clearRowExcept(oldPosition, newPosition, p, t)
                    || board.clearDiagonalExcept(oldPosition, newPosition, p, t)
        );
	}
	
	public String toString() {
		if(isWhite) {
			return "Q";
		} else {
			return "q";
		}
	}
}
