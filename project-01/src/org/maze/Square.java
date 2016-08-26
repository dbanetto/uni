package org.maze;

import com.sun.istack.internal.NotNull;
import com.sun.istack.internal.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

class Square {
    private final Tile tile;
    private final int x;
    private final int y;
    private final AtomicReference<Crawler> person;


    private Square[] neighbours;

    private Map<Direction, Mark> marks;



    public Square(@NotNull Tile tile, int x, int y) {
        this.tile = tile;
        this.marks = new HashMap<>();
        this.x = x;
        this.y = y;
        this.person = new AtomicReference<>(null);
    }

    public void setNeighbours(@Nullable Square north, @Nullable Square east,
                              @Nullable Square south, @Nullable Square west) {
        neighbours = new Square[4];
        neighbours[Direction.North.toInt()] = north;
        neighbours[Direction.East.toInt()] = east;
        neighbours[Direction.South.toInt()] = south;
        neighbours[Direction.West.toInt()] = west;
    }

    public @NotNull Tile getTile() {
        return tile;
    }

    public Map<Direction, Mark> getMarks() {
        return marks;
    }

    public @Nullable Square getNeighbour(@NotNull Direction direction) {
        if (neighbours == null) {
            throw new IllegalStateException();
        }
        return neighbours[direction.toInt()];
    }

    public List<Direction> possiblePaths() {
        List<Direction> possible = new ArrayList<>(4);
        for (Direction dir : Direction.values()) {
            Square to = getNeighbour(dir);
            if (to == null) {
                continue;
            }
            if (to.getTile() == Tile.SPACE) {
                possible.add(dir);
            }
        }
        return possible;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public AtomicReference<Crawler> getPerson() {
        return person;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Square square = (Square) o;

        if (x != square.x) return false;
        return y == square.y;

    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
    }

    @Override
    public String toString() {
        return person.get() == null ? tile.toString() :  (person.get().isGolden() ? "G" : "C");
    }

}
