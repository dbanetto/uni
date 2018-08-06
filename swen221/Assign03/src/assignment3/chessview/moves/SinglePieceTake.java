package assignment3.chessview.moves;

import assignment3.chessview.*;
import assignment3.chessview.pieces.Piece;

public class SinglePieceTake extends SinglePieceMove {
	private Piece isTaken;
	
	public SinglePieceTake(Piece piece, Piece isTaken, Position oldPosition, Position newPosition) {
		super(piece,oldPosition,newPosition);
		this.isTaken = isTaken;
	}
	
	public boolean isValid(Board board) {
		if (piece.isWhite() == isTaken.isWhite()) { return false; }
		return piece.isValidMove(oldPosition, newPosition, isTaken, board);
	}
	
	public String toString() {
		return pieceChar(piece) + oldPosition + "x" + pieceChar(isTaken) + newPosition; 
	}	
}
