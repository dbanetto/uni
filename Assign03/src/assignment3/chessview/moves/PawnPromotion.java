package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.*;

/**
 * This represents a pawn promotion.
 * @author djp
 *
 */
public class PawnPromotion implements MultiPieceMove {
	private Piece promotion;
	private Move move;
	
	public PawnPromotion(SinglePieceMove move, Piece promotion) {
		this.promotion = promotion;
		this.move = move;
	}
	
	public boolean isWhite() {
		return promotion.isWhite();
	}
	
	public boolean isValid(Board board) {
		if (!(move instanceof SinglePieceMove)) {
			return false;
		}
		SinglePieceMove pieceMove = (SinglePieceMove)move;

		if (!(pieceMove.piece() instanceof Pawn)) {
			return false;
		}
		if (!move.isValid(board)) {
			return false;
		}

		Pawn toPromote = (Pawn)pieceMove.piece();
		if (pieceMove.newPosition().row() != (promotion.isWhite() ? 8 : 1)) {
			return false;
		}

		return true;
	}
	
	public void apply(Board board) {
		if (!(move instanceof SinglePieceMove)) {
			return;
		}
		SinglePieceMove pieceMove = (SinglePieceMove)move;
		board.setPieceAt(pieceMove.newPosition(), promotion);
		board.setPieceAt(pieceMove.oldPosition(), null);
	}
	
	public String toString() {
		return super.toString() + "=" + SinglePieceMove.pieceChar(promotion);
	}

	public Piece getPromotion() {
		return promotion;
	}

	public Move getMove() {
		return move;
	}
}
