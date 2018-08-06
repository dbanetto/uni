package assignment2;

import java.awt.*;
import java.util.*;
import java.util.List;

import maze.*;
import maze.gui.MazeWindow;

/**
 * An any implementation of the left walker, which you need to complete
 * following the notes.
 * 
 */
public class LeftWalker extends Walker {
	
	private Direction facing;
    private Point location; // location relative to origin
    private boolean wallAlign;
    private boolean wallCharge = false;
    private MapNode currentNode;
    private Map<Point, MapNode> map;

	public LeftWalker() {
		super("Left Walker");
        facing = Direction.NORTH;
        location = new Point(0, 0);
        wallAlign = true;

        currentNode = new MapNode();
        map = new HashMap<Point, MapNode>();
        map.put(location, currentNode);
	}

	protected Direction move(View v) {
        updateMap(v, currentNode, location);

        if (wallCharge) {
            if (!v.mayMove(facing)) {
                wallCharge = false;
                wallAlign = true;
            }
        }

        if (wallAlign) {

            Direction toFace = alignWithWall(v);
            if (toFace != null) {
                facing = toFace;
            }
            wallAlign = false;
        } else {


            if (wallCharge) {
                // charge forward
            } else if (v.mayMove(leftOfDirection(facing))) { // turn Left
                facing = leftOfDirection(facing);
            } else if (v.mayMove(facing)) { // turn move forward, leave facing alone
            } else if (v.mayMove(rightOfDirection(facing))) {
                facing = rightOfDirection(facing);
            } else {
                facing = behindDirection(facing);
            }

            if (currentNode.visited) {
                if (currentNode.exited.contains(facing)) {
                    Direction toFace = facing;
                    if (currentNode.paths == 1) {

                    } else if (currentNode.paths > currentNode.exited.size()) {
                        Set<Direction> possible = getPaths(v, currentNode, toFace);
                        possible.remove(currentNode.exited);
                        for (int i = 0; i < 4; i++) {
                            toFace = rightOfDirection(toFace);
                            if (possible.contains(toFace)) {
                                break;
                            }
                        }
                        wallCharge = true;
                        wallAlign = true;
                    } else if (currentNode.paths == currentNode.exited.size()) {
                        toFace = currentNode.previous;
                        for (int i = 0; i < 4; i++) {
                            toFace = rightOfDirection(toFace);
                            if (v.mayMove(toFace)) {
                                break;
                            }
                        }
                        wallAlign = false;
                    } else {
                        toFace = rightOfDirection(toFace);
                    }

                    facing = toFace;
                }
            }
        }


        if (v.mayMove(facing)) {
            updateLocation(facing, location);
            currentNode.exited.add(facing);
            currentNode.previous = facing;
            currentNode.visited = true;
            currentNode = map.get(location);
        }
        return facing;
	}

    /**
     * Map the surrounding area for walls and possible routes
     *
     * @param v View to check for walls
     */
    private void updateMap(View v, MapNode node, Point loc) {
        Direction toFace = Direction.NORTH;
        for (int i = 0; i < 4; i++) {
            if (v.mayMove(toFace)) {
                Point pt = new Point(loc);
                updateLocation(toFace, pt);
                if (!map.containsKey(pt)) {
                    map.put(pt,  new MapNode());
                    node.paths++;
                }

            }
            toFace = rightOfDirection(toFace); // clockwise
        }
    }

    private Set<Direction> getPaths(View v, MapNode node, Direction startDir) {
        Direction toFace = startDir;
        Set<Direction> toReturn = new HashSet<Direction>();
        for (int i = 0; i < 4; i++) {
            if (v.mayMove(toFace)) {
                toReturn.add(toFace);
            }
            toFace = rightOfDirection(toFace); // clockwise
        }
        return toReturn;
    }

    /**
     * Update a given point with what Direction that was moved
     *
     * @param toMove Direction to move
     * @param point Point to be updated with new location
     */
    private void updateLocation(Direction toMove, Point point) {
        if (toMove.equals(Direction.NORTH)) {
            point.translate(0, -1);
        } else if (toMove.equals(Direction.SOUTH)) {
            point.translate(0, 1);
        } else if (toMove.equals(Direction.WEST)) {
            point.translate(-1, 0);
        } else if (toMove.equals(Direction.EAST)) {
            point.translate(1, 0);
        } else {
            throw new IllegalArgumentException();
        }
    }

    private Direction alignWithWall(View v) {
        return alignWithWall(v, 6);
    }

    /**
     * Get direction to face to align with clockwise wall
     *
     * uses facing as initial direction
     *
     * @param v View
     * @param cycles how many turns should be checked
     * @return The direction to face to have the closest clockwise wall to the left,
     * if none is found null is returned
     */
    private Direction alignWithWall(View v, int cycles) {
        Direction toFace = facing;
        Direction hasLeftWall = null;
        for (int i = 0; i < cycles; i++) {
            if (v.mayMove(toFace) && !v.mayMove(leftOfDirection(toFace))) {
                hasLeftWall = toFace;
                break;
            }
            toFace = rightOfDirection(toFace); // clockwise
        }
        return hasLeftWall;
    }

    /**
     * Checks if there is any walls around the given view
     *
     * @param v View of Map
     * @return has no walls around
     */
    private boolean noWallsAround(View v) {
        Direction toFace = rightOfDirection(facing);
        for (int i = 0; i < 4; i++) {
            if (!v.mayMove(toFace)) {
                return false;
            }
            toFace = rightOfDirection(toFace); // clockwise
        }
        return true;
    }

    /**
     * Returns the left of a given Direction
     *
     * @param facing Direction to be left of
     * @return Left of given Direction
     */
    private Direction leftOfDirection(Direction facing) {
        if (facing.equals(Direction.NORTH)) {
            return Direction.WEST;
        } else if (facing.equals(Direction.WEST)) {
            return Direction.SOUTH;
        } else if (facing.equals(Direction.SOUTH)) {
            return Direction.EAST;
        } else if (facing.equals(Direction.EAST)) {
                return Direction.NORTH;
        } else {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Returns the right of a given Direction
     *
     * @param facing Direction to be right of
     * @return Right of given Direction
     */
    private Direction rightOfDirection(Direction facing) {
        if ( facing.equals(Direction.NORTH)) {
            return Direction.EAST;
        } else if (facing.equals(Direction.EAST)) {
            return Direction.SOUTH;
        } else if (facing.equals(Direction.SOUTH)) {
            return Direction.WEST;
        } else if (facing.equals(Direction.WEST)) {
            return Direction.NORTH;
        } else {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Returns the behind of a given Direction
     *
     * @param facing Direction to be behind of
     * @return Behind of given Direction
     */
    private Direction behindDirection(Direction facing) {
        if ( facing.equals(Direction.NORTH)) {
            return Direction.SOUTH;
        } else if (facing.equals(Direction.SOUTH)) {
            return Direction.NORTH;
        } else if (facing.equals(Direction.EAST)) {
            return Direction.WEST;
        } else if (facing.equals(Direction.WEST)) {
            return Direction.EAST;
        } else {
            throw new IllegalArgumentException();
        }
    }

    private class MapNode {
        boolean visited;
        Set<Direction> exited;
        Direction previous;
        int paths;

        public MapNode() {
            visited = false;
            exited = new HashSet<Direction>();
            previous = null;
            paths = 0;
        }

        @Override
        public String toString() {
            return "MapNode{" +
                    "visited=" + visited +
                    ", paths=" + paths +
                    ", exited=" + exited +
                    ", previous=" + previous +
                    '}';
        }
    }
}