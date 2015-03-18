package assignment2;

import java.awt.*;
import java.util.*;
import static maze.Direction.*;
import maze.*;

/**
 * An any implementation of the left walker, which you need to complete
 * following the notes.
 * 
 */
public class LeftWalker extends Walker {
	
	private  Direction facing;
    private Point location; // location relative to origin
    private boolean findNewWall;

	public LeftWalker() {
		super("Left Walker");
        facing = null;
        location = new Point(0, 0);
        findNewWall = false;
	}

	protected Direction move(View v) {
        if (facing == null) {
            facing = Direction.NORTH;
            facing = alignWithWall(v);

        } else if (findNewWall) {
            if (!v.mayMove(facing)) {
                findNewWall = false;
                facing = rightOfDirection(facing);
            }
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

            if (location.equals(new Point(0,0))) {
                System.out.print("Special Case: ");
                Direction oldFacing = facing;

                if (!noWallsAround(v)) {
                    if (v.mayMove(rightOfDirection(facing))) {
                        facing = leftOfDirection(facing);
                        if (v.mayMove(facing)) {
                            facing = rightOfDirection(facing);
                        } else {
                            facing = behindDirection(facing);
                        }
                        System.out.println("Bounce");
                    } else {
                        System.out.println("FORWARD");
                    }
                } else {
                    facing = Direction.SOUTH;
                    System.out.println("South");
                }
                findNewWall = true;
            }
        }
        if (v.mayMove(facing)) {
            updateLocation(facing);
        }
        System.out.println(location);
        return facing;
	}

    private void updateLocation(Direction toMove) {
        if (facing.equals(Direction.NORTH)) {
            location.translate(0,1);
        } else if (facing.equals(Direction.SOUTH)) {
            location.translate(0,-1);
        } else if (facing.equals(Direction.WEST)) {
            location.translate(-1,0);
        } else if (facing.equals(Direction.EAST)) {
            location.translate(1,0);
        } else {
            throw new IllegalArgumentException();
        }
    }

    private Direction alignWithWall(View v) {
        return alignWithWall(v, 4);
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
        if (hasLeftWall == null) {
            toFace = Direction.NORTH;
        } else {
            toFace = hasLeftWall;
        }
        return toFace;
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
}