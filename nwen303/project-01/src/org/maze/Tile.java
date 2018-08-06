package org.maze;

public enum Tile {
    WALL,
    SPACE;

    @Override
    public String toString() {
        switch (this) {
            case WALL:
                return "X";
            case SPACE:
                return " ";
            default:
                return "";
        }
    }
}
