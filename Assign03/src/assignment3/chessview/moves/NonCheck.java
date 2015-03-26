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
		return move.isValid(board);
	}
	
	public void apply(Board board) {
		move.apply(board);
	}
	
	public String toString() {
		return move.toString();
	}
}
