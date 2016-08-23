package org.maze;

import com.sun.istack.internal.NotNull;
import com.sun.istack.internal.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Square {
    private final Tile tile;

    private Square[] neighbours;

    private Map<Direction, Mark> marks;

    public Square(@NotNull Tile tile) {
        this.tile = tile;
        this.marks = new HashMap<>();
    }

    public void setNeighbours(@Nullable Square north, @Nullable Square east,
                              @Nullable Square south, @Nullable Square west) {
        neighbours = new Square[4];
        neighbours[Direction.North.toInt()] = north;
        neighbours[Direction.East.toInt()] = east;
        neighbours[Direction.South.toInt()] = south;
        neighbours[Direction.West.toInt()] = west;

        // init marks map
        possiblePaths().forEach(d -> marks.put(d, Mark.NONE));
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

    @Override
    public String toString() {
        return "Square{" +
                "tile=" + tile +
                '}';
    }

}
