package assignment4.shapes;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Implements the Shape class for a Rectangle
 */
public class Rectangle implements Shape {
    private int x, y, width, height;
    private Rectangle boundBox = null;
    private List<AppliedShape> applied;

    /**
     * Create a Rectangle object
     *
     * @param x horizontal coordinate
     * @param y vertical coordinate
     * @param width width of rectangle
     * @param height height of rectangle
     */
    public Rectangle(int x, int y, int width, int height) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;

        if (this.width <= 0) {
            throw new IllegalArgumentException("Width must be greater than 0");
        }
        if (this.height <= 0) {
            throw new IllegalArgumentException("Height must be greater than 0");
        }
        applied = new ArrayList<>();
        boundBox = new Rectangle(x,y,width,height, true);
    }

    /**
     * Internal constructor to prevent circular dependency
     * when making a bounding box
     *
     * @param x horizontal coordinate
     * @param y vertical coordinate
     * @param width width of rectangle
     * @param height height of rectangle
     * @param noBound value does not matter
     */
    private Rectangle(int x, int y, int width, int height, boolean noBound) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;

        if (this.width <= 0) {
            throw new IllegalArgumentException("Width must be greater than 0");
        }
        if (this.height <= 0) {
            throw new IllegalArgumentException("Height must be greater than 0");
        }
    }

    /**
     * Copy Rectangle, set bounding box and add an AppliedShape
     *
     * @param rect Rectangle to be copied
     * @param boundBox Bounding box of new shape
     * @param applied applied shape to be added
     */
    private Rectangle(Rectangle rect, Rectangle boundBox, AppliedShape applied) {
        this.x = rect.x;
        this.y = rect.y;
        this.width = rect.width;
        this.height = rect.height;

        // Deeper copy
        this.applied = new ArrayList<>();
        Collections.copy(rect.applied, this.applied);

        this.applied.add(applied);
        this.boundBox = boundBox;
    }

    /**
     * @see super.contains
     */
    @Override
    public boolean contains(int x, int y) {
        boolean contains = (this.x <= x && this.y <= y && (this.x + this.width) > x && (this.y + this.height) > y);
        // Simple case of no applied shapes
        if (applied.size() == 0) {
            return contains;
        } else {
            // Check each applied shape in order of changes
            for (AppliedShape shape : this.applied) {
                switch (shape.type) {
                    case UNION:
                        contains |= shape.shape.contains(x, y); // OR
                        break;
                    case INTERSECTION:
                        contains &= shape.shape.contains(x, y); // AND
                        break;
                    case DIFFERENCE:
                        contains &= !shape.shape.contains(x, y); // NAND
                        break;
                }
            }
            return contains;
        }
    }

    /**
     * @see super.boundingBox
     */
    @Override
    public Rectangle boundingBox() {
        return boundBox;
    }

    /**
     * @see super.union
     */
    @Override
    public Shape union(Shape other) {
        Rectangle otherBounds = other.boundingBox();

        int thisRight   = this.boundBox.getX() + this.boundBox.getWidth();
        int thisBottom  = this.boundBox.getY() + this.boundBox.getHeight();
        int otherRight  = otherBounds.getX() + otherBounds.getWidth();
        int otherBottom = otherBounds.getY() + otherBounds.getHeight();

        // Expand bounding box
        int bx = Math.min(this.boundBox.getX(), otherBounds.getX());
        int by = Math.min(this.boundBox.getY(), otherBounds.getY());
        int bw = Math.max(thisRight, otherRight) - bx;
        int bh = Math.max(thisBottom, otherBottom) - by;

        boundBox = Rectangle.MakeBoundBox(bx, by, bw, bh);
        return new Rectangle(this, boundBox, new AppliedShape(other, AppliedMethod.UNION));
    }

    /**
     * @see super.intersected
     */
    @Override
    public Shape intersected(Shape other) {
        Rectangle otherBounds = other.boundingBox();

        // Shrink bounding box
        int thisRight   = this.boundBox.getX() + this.boundBox.getWidth();
        int thisBottom  = this.boundBox.getY() + this.boundBox.getHeight();
        int otherRight  = otherBounds.getX() + otherBounds.getWidth();
        int otherBottom = otherBounds.getY() + otherBounds.getHeight();


        // Expand bounding box
        int bx = Math.max(this.boundBox.getX(), otherBounds.getX());
        int by = Math.max(this.boundBox.getY(), otherBounds.getY());
        int bw = Math.min(thisRight, otherRight) - bx;
        int bh = Math.min(thisBottom, otherBottom) - by;

        boundBox = Rectangle.MakeBoundBox(bx, by, bw, bh);
        return new Rectangle(this, boundBox, new AppliedShape(other, AppliedMethod.INTERSECTION));
    }

    /**
     * @see super.difference
     */
    @Override
    public Shape difference(Shape other) {
        Rectangle ob = other.boundingBox();

        int bx = this.boundBox.getX();
        int by = this.boundBox.getY();
        int bw = this.boundBox.getWidth();
        int bh = this.boundBox.getHeight();

        // top intersect case
        if (ob.getY() <= boundBox.getY()) {
            if (ob.getX() <= boundBox.getX() && ob.getX() + ob.getWidth() >= boundBox.getX() + boundBox.getWidth()) {
                by = Math.max(boundBox.getY(), ob.getY() + ob.getHeight());
            }
        } else {
            // bottom intersect case
            if (ob.getX() <= boundBox.getX() && ob.getX() + ob.getWidth() >= boundBox.getX() + boundBox.getWidth()) {
                bh = Math.min(boundBox.getY() + boundBox.getHeight(), ob.getY());
            }
        }

        // left intersect case
        if (ob.getX() <= boundBox.getX()) {
            if (ob.getY() <= boundBox.getY() && ob.getY() + ob.getHeight() >= boundBox.getY() + boundBox.getHeight()) {
                bx = Math.max(boundBox.getX(), ob.getX() + ob.getWidth());
            }
        } else {
            // right intersect case
            if (ob.getY() <= boundBox.getY() && ob.getY() + ob.getHeight() >= boundBox.getY() + boundBox.getHeight()) {
                bw = Math.min(boundBox.getX() + boundBox.getWidth(), ob.getX());
            }
        }

        boundBox = Rectangle.MakeBoundBox(bx, by, bw, bh);
        return new Rectangle(this, boundBox, new AppliedShape(other, AppliedMethod.DIFFERENCE));
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Rectangle rectangle = (Rectangle) o;

        return x == rectangle.x && y == rectangle.y && width == rectangle.width && height == rectangle.height;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        result = 31 * result + width;
        result = 31 * result + height;
        return result;
    }

    // Getters
    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getWidth() {
        return width;
    }

    public int getHeight() {
        return height;
    }

    /**
     *
     * @param x horizontal coordinate
     * @param y vertical coordinate
     * @param width width of rectangle
     * @param height height of rectangle
     * @return Bounding Box of given dimensions
     */
    public static Rectangle MakeBoundBox(int x, int y, int width, int height) {
        // allows user to make BoundBox
        return new Rectangle(x, y, width, height, true);
    }

    /**
     * Wrapper class to bind Shape and its method and shape
     */
    private class AppliedShape {
        final Shape shape;
        final AppliedMethod type;

        public AppliedShape(Shape shape, AppliedMethod type) {
            this.shape = shape;
            this.type = type;
        }
    }

    /**
     * The method the shape is applied
     */
    private enum AppliedMethod {
        UNION,        // AND
        INTERSECTION, // OR
        DIFFERENCE,   // NAND
    }
}
