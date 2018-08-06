// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;

/** 
 * O's and X's board.
*/

public class OandXBoard{

    // The dimensions of the board
    private static final int Dimen = 3;
    private static final double CellSize = 100;

    public static final double Left = 50;
    public static final double Top = 50;
    public static final double Right = Left + Dimen * CellSize;
    public static final double Bot = Top + Dimen * CellSize;
    private static final int FontSize = (int) (CellSize*0.8);

    /** The state of the board, as an array of Symbols:
        O, X, or null for empty.
    */
    private Symbol[][] cells = new Symbol[Dimen][Dimen];
    
    /**
     * Reset the board to be clear of all symbols
     */
    public void reset(){
        cells = new Symbol[Dimen][Dimen];
    }

    /**
     * Is the point (x,y) on the board
     */
    public boolean on(double x, double y){
        return (y>=Top && y < Bot  && x>=Left && x < Right);
    }
    
    /**
     * Return the row/col corresponding to the point x,y.
     */
    public int[] rowCol(double x, double y){
        int row = (int) ((y-Top)/CellSize);
        int col = (int) ((x-Left)/CellSize);
        return new int[]{row, col};
    }

    /**
     * Place the given symbol at the given row/col
     * Returns true if symbol is placed successfully.
     * Returns false if the position is invalid, or
     * the symbol is invalid, or there is already a symbol there
     */
    public boolean place(Symbol symbol, int row, int col){
        if (row<0 || row >=Dimen || col<0 || col >=Dimen) { return false; }
        if (!symbol.equals(Symbol.X) && !symbol.equals(Symbol.O)){ return false; }
        if (cells[row][col]!= null) { return false; }
        cells[row][col] = symbol;
        return true;
    }
    
    /**
     * Check whether the play at row/col resulted in a win for the player.
     */
    public boolean checkWin(int row, int col){
        if (row<0 || row >=Dimen || col<0 || col >=Dimen) { return false; }
        if (cells[row][col]==null) return false;
        // check the row
        if (cells[row][0]==(cells[row][1]) &&
            cells[row][0]==(cells[row][2])){
            return true;
        }
        // check the column
        if (cells[0][col]==(cells[1][col]) &&
            cells[0][col]==(cells[2][col])){
            return true;
        }
        // check diagonals
        if ((row==col &&  // on 0,0 to 2,2 diagonal
             cells[1][1]==cells[0][0] &&
             cells[1][1]==cells[2][2]) ||
            (row==(Dimen-col) && // on 0,2 to 2,0 diagonal
             cells[1][1]==cells[0][2] &&
             cells[1][1]==cells[2][0])){
            return true;
        }
        return false;
    }

    /**
     * Check whether the game is over (no more cells to play in)
     */
    public boolean finished(){
        for (int row=0; row<Dimen; row++){
            for (int col=0; col<Dimen; col++){
                if (cells[row][col]==null){
                    return false;    // one cell has a space, so not over yet.
                }
            }
        }
        return true;
    }
        

    /**
     * Redraw the board. Assumes that the graphics pane has been cleared
     */
    public void draw(){
        UI.setFontSize(FontSize);
        double y = Top;
        for (int row=0; row<Dimen; row++){
            double x = Left;
            for (int col=0; col<Dimen; col++){
                UI.drawRect(x, y, CellSize, CellSize);
                if (cells[row][col]!= null){
                    UI.drawString(cells[row][col].toString(), x+CellSize*.25, y+CellSize*.8);
                }
                x += CellSize;
            }
            y += CellSize;
        }
    }

    public static void main(String[] args){
        OandX game = new OandX();
    }

}
