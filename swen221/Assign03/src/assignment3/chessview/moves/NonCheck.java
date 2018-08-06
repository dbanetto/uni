package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.*;

public class NonCheck implements Move {
	private MultiPieceMove move;	
	
	public NonCheck(MultiPieceMove move) {
		this.move = move;		
	}
	
	public MultiPieceMove move() {
		return move;
	}
	
	public boolean isWhite() {
		return move.isWhite();
	}
	
	public boolean isValid(Board board) {
		if (!move.isValid(board)) {
			return false;
		}

		// Allow an override of what move we playing forward
		Move toCheck = move;
		if (move instanceof  EnPassant) {
			toCheck = ((EnPassant)move).getMove();
		}

		// Special case for PawnPromotion
		if (move instanceof  PawnPromotion) {
			// play forward the move and check if it goes into check
			// since it is NonCheck we are not allowing it
			PawnPromotion pawnPromotion = (PawnPromotion) (move);
			toCheck = pawnPromotion.getMove();
			SinglePieceMove movement = (SinglePieceMove) toCheck;

			Piece newSpot = board.pieceAt(movement.newPosition());

			// play forward
			board.setPieceAt(movement.newPosition(), pawnPromotion.getPromotion());
			board.setPieceAt(movement.oldPosition(), null);

			boolean inCheck = board.isInCheck(toCheck.isWhite());
			// Hard check if the promotion piece will be in check
			if (pawnPromotion.getPromotion().isValidMoveLookAhead(movement.newPosition(), board.findKing(!move.isWhite()), board.pieceAt(board.findKing(!move.isWhite())), board)) {
				inCheck = true;
			}

			// reset field
			board.setPieceAt(movement.newPosition(), newSpot);
			board.setPieceAt(movement.oldPosition(), movement.piece());

			if (inCheck) {
				return false;
			}

		} else if (toCheck instanceof SinglePieceMove) {
			// play forward the move and check if it goes into check
			// since it is NonCheck we are not allowing it
			SinglePieceMove movement = (SinglePieceMove)toCheck;

			Piece newSpot = board.pieceAt(movement.newPosition());

			board.setPieceAt(movement.newPosition(), movement.piece());
			board.setPieceAt(movement.oldPosition(), null);

			boolean inCheck = board.isInCheck(toCheck.isWhite());

			// reset field
			board.setPieceAt(movement.newPosition(), newSpot);
			board.setPieceAt(movement.oldPosition(), movement.piece());

			if (inCheck) {
				return false;
			}
		}
		return true;
	}
	
	public void apply(Board board) {
		move.apply(board);
	}
	
	public String toString() {
		return move.toString();
	}
}
