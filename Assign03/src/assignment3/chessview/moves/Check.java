package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.*;

/**
 * This represents a "check move". Note that, a check move can only be made up
 * from an underlying simple move; that is, we can't check a check move.
 * 
 * @author djp
 * 
 */
public class Check implements Move {
	private MultiPieceMove move;	
	
	public Check(MultiPieceMove move) {
		this.move = move;		
	}
	
	public MultiPieceMove move() {
		return move;
	}
	
	public boolean isWhite() {
		return move.isWhite();
	}
	
	public boolean isValid(Board board) {
		if (move instanceof PawnPromotion) {
			PawnPromotion moving = (PawnPromotion)(move);
			Position opKingPos = board.findKing(!isWhite());
			SinglePieceMove promotionMove = (SinglePieceMove)moving.getMove();
			King king = (King)board.pieceAt(opKingPos);

			return moving.isValid(board) && moving.getPromotion().isValidMoveLookAhead(promotionMove.newPosition, opKingPos, king, board);
		} else if (move instanceof  SinglePieceTake) {
			SinglePieceTake moving = (SinglePieceTake)(move);
			Position opKingPos = board.findKing(!isWhite());
			King king = (King)board.pieceAt(opKingPos);

			return moving.isValid(board) && moving.piece.isValidMoveLookAhead(moving.newPosition, opKingPos, king, board);
		} else if (move instanceof SinglePieceMove) {
			SinglePieceMove moving = (SinglePieceMove)(move);
			Position opKingPos = board.findKing(!isWhite());
			King king = (King)board.pieceAt(opKingPos);

			return moving.isValid(board) && moving.piece.isValidMoveLookAhead(moving.newPosition, opKingPos, king, board);
		}
		return false;
	}
	
	public void apply(Board board) {
		move.apply(board);
	}
	
	public String toString() {
		return move.toString() + "+";
	}
}
