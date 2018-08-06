package org.maze;

enum Direction {
    North,
    East,
    South,
    West;

    public int toInt() {
        switch (this) {
            case North:
                return 0;
            case East:
                return 1;
            case South:
                return 2;
            case West:
                return 3;
            default:
                throw new IllegalArgumentException();
        }
    }

    public Direction turnRight() {
        switch (this) {
            case North:
                return East;
            case East:
                return South;
            case South:
                return West;
            case West:
                return North;
            default:
                throw new IllegalArgumentException();
        }
    }

    public Direction turnAround() {
        return this.turnRight().turnRight();
    }

    public Direction turnLeft() {
        return this.turnRight().turnRight().turnRight();
    }
}
