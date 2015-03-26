package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.*;

public class Castling implements MultiPieceMove {	
	private boolean kingSide;
	private boolean isWhite;
	
	public Castling(boolean isWhite, boolean kingSide) {		
		this.kingSide = kingSide;
		this.isWhite = isWhite;
	}
	
	public boolean isWhite() {
		return this.isWhite;
	}
	
	public void apply(Board board) {
		int rookCol = kingSide ? 8 : 1;
		int row = isWhite ? 1 : 8;

		int kingCol = 5;
		Position kingPos =  new Position(row, kingCol);
		Position rookPos = new Position(row, rookCol);
		Piece king = board.pieceAt(kingPos);
		Piece rook = board.pieceAt(rookPos);

		int newKingCol = 3;
		if (kingSide) {
			newKingCol = 7;
		}
		Position newKingPos =  new Position(row, newKingCol);
		Position newRookNewPos = new Position(row, newKingCol + (kingSide ? -1 : 1 ));

		board.move(kingPos, newKingPos);
		board.move(rookPos, newRookNewPos);
	}
	
	public boolean isValid(Board board) {

		int rookCol = kingSide ? 8 : 1;
		int row = isWhite ? 1 : 8;

		int kingCol = 5;
		Position kingPos =  new Position(row, kingCol);
		Position rookPos = new Position(row, rookCol);

		Piece king = board.pieceAt(kingPos);
		Piece rook = board.pieceAt(rookPos);

		// test if moved

		if (king == null || rook == null) {
			return false;
		}

		if (!(king instanceof King) || !(rook instanceof Rook)) {
			return false;
		}

		if (!board.clearRowExcept(rookPos, kingPos, king, rook)) {
			return false;
		}

		if (king.hasMoved() || rook.hasMoved()) {
			return false;
		}

		return true;
	}		
	
	public String toString() {
		if(kingSide) {
			return "O-O";
		} else {
			return "O-O-O";
		}
	}
}
