package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.*;

/**
 * This represents an "en passant move" --- http://en.wikipedia.org/wiki/En_passant.
 * 
 * @author djp
 * 
 */
public class EnPassant implements MultiPieceMove {
	private SinglePieceMove move;
	public EnPassant(SinglePieceMove move) {
		this.move = move;
	}
	
	public boolean isWhite() {
		return false;
	}
	
	public boolean isValid(Board board) {
		int enPassRow = move.oldPosition().row();
		int col = move.newPosition().column();

		Piece enPassed = board.pieceAt(new Position(enPassRow, col));

		if (!(move.piece instanceof Pawn)) {
			return false;
		}

		if (!(enPassed instanceof Pawn)) {
			return false;
		}
		if (!enPassed.getLastPostiion().equals(new Position(enPassRow + (enPassed.isWhite() ? -2 : 2), col))) {
			return false;
		}

		return true;
	}
	
	public void apply(Board board) {
		int enPassRow = move.oldPosition().row();
		int col = move.newPosition().column();

		board.move(move.oldPosition(), move.newPosition());
		board.setPieceAt(new Position(enPassRow, col), null);
	}
	
	public String toString() {
		return "ep";
	}
}
