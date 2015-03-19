package assignment2;

import java.awt.*;
import java.util.*;
import java.util.List;

import maze.*;

/**
 * An any implementation of the left walker, which you need to complete
 * following the notes.
 * 
 */
public class LeftWalker extends Walker {
	
	private Direction facing;
    private Direction oldFacing;
    private Point location; // location relative to origin
    private boolean wallAlign;
    private MapNode currentNode;
    private Map<Point, MapNode> map;

	public LeftWalker() {
		super("Left Walker");
        facing = Direction.NORTH;;
        location = new Point(0, 0);
        wallAlign = true;

        currentNode = new MapNode();
        map = new HashMap<Point, MapNode>();
        map.put(location, currentNode);
	}

	protected Direction move(View v) {
        updateMap(v);
        System.out.println("At " + location.x + "," + location.y);

        if (wallAlign) {

            Direction toFace = alignWithWall(v);
            if (toFace != null) {
                facing = toFace;
            }
            wallAlign = false;
        } else {
            if (v.mayMove(leftOfDirection(facing))) { // turn Left
                facing = leftOfDirection(facing);
            } else if (v.mayMove(facing)) { // turn move forward
                //
            } else if (v.mayMove(rightOfDirection(facing))) {
                facing = rightOfDirection(facing);
            } else {
                facing = behindDirection(facing);
            }

            if (currentNode.visited) {
                if (currentNode.exited.contains(facing)) {
                    System.out.println("Hmm I have tried this way before");
                    Direction toFace = facing;
                    do {
                        toFace = rightOfDirection(toFace);
                    } while ((currentNode.exited.contains(toFace) || !v.mayMove(toFace)) && toFace != facing);
                    if (!noWallsAround(v) && toFace == facing) {
                        wallAlign = true;
                    }
                    System.out.println("Changing from " + facing + " to " + toFace);
                    System.out.println("What would this want: " + getAvailable(location, facing, currentNode));
                    facing = toFace;
                }

                System.out.println("hey, I have been here lets try: " + facing);

            }
        }


        if (v.mayMove(facing)) {
            updateLocation(facing, location);
            System.out.println("On Node: " + currentNode);
            currentNode.exited.add(facing);
            currentNode.previous = facing;
            currentNode.visited = true;
            currentNode = map.get(location);

            System.out.println("Moving to : " + currentNode + " via " + facing);
            System.out.println("Moving to : " + location.x + "," + location.y);
        }
        return facing;
	}

    private void updateMap(View v) {
        Direction toFace = Direction.NORTH;
        for (int i = 0; i < 4; i++) {
            if (v.mayMove(toFace)) {
                Point pt = (Point)location.clone();
                updateLocation(toFace, pt);

                if (!map.containsKey(pt)) {
                    map.put(pt,  new MapNode());
                }
            }
            toFace = rightOfDirection(toFace); // clockwise
        }
    }

    private void updateLocation(Direction toMove, Point point) {
        if (toMove.equals(Direction.NORTH)) {
            point.translate(0, 1);
        } else if (toMove.equals(Direction.SOUTH)) {
            point.translate(0, -1);
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

    private List<Direction> getAvailable(Point pt, Direction starting, MapNode node) {
        List<Direction> avail = new ArrayList<Direction>(4);
        Direction toFace = leftOfDirection(starting);
        for (int i = 0; i < 4; i++) {
            Point loc = (Point)pt.clone();
            updateLocation(toFace, loc);
            if (map.containsKey(loc)) {
                MapNode srrnd = map.get(loc);
                if (!srrnd.visited) {
                    avail.add(toFace);
                }
            }
            toFace = rightOfDirection(toFace);
        }
        toFace = rightOfDirection(starting);
        for (int i = 0; i < 4; i++) {
            Point loc = (Point)pt.clone();
            updateLocation(toFace, loc);
            if (map.containsKey(loc)) {
                MapNode srrnd = map.get(loc);
                if (!node.exited.contains(toFace)) {
                    avail.add(toFace);
                }
            }
            toFace = rightOfDirection(toFace);
        }
        toFace = rightOfDirection(starting);
        for (int i = 0; i < 4; i++) {
            Point loc = (Point)pt.clone();
            updateLocation(toFace, loc);
            if (map.containsKey(loc)) {

                    avail.add(toFace);

            }
            toFace = rightOfDirection(toFace);
        }

        return avail;
    }

    private class MapNode {
        boolean visited;
        Set<Direction> exited;
        Direction previous;

        public MapNode() {
            visited = false;
            exited = new HashSet<Direction>();
            previous = null;
        }

        @Override
        public String toString() {
            return "MapNode{" +
                    "visited=" + visited +
                    ", exited=" + exited +
                    ", previous=" + previous +
                    '}';
        }
    }
}