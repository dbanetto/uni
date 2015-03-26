package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.*;

/**
 * This represents an "en passant move" --- http://en.wikipedia.org/wiki/En_passant.
 * 
 * @author djp
 * 
 */
public class EnPassant implements Move {
	public EnPassant(SinglePieceMove move) {				
	}
	
	public boolean isWhite() {
		return false;
	}
	
	public boolean isValid(Board board) {		
		return true;
	}
	
	public void apply(Board board) {		
	}
	
	public String toString() {
		return "ep";
	}
}
