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

		if (move instanceof SinglePieceMove) {
			SinglePieceMove movement = (SinglePieceMove)move;

			Piece newSpot = board.pieceAt(movement.newPosition());

			// play forward
			board.setPieceAt(movement.newPosition(), movement.piece());
			board.setPieceAt(movement.oldPosition(), null);

			boolean stillInCheck = board.isInCheck(move.isWhite());

			// reset field
			board.setPieceAt(movement.newPosition(), newSpot);
			board.setPieceAt(movement.oldPosition(), movement.piece());
			if (stillInCheck) {
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
