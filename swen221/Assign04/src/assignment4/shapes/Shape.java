package assignment4.shapes;

/**
 * This class captures the abstract notion of a Shape.
 * @author djp
 *
 */
public interface Shape {
	/**
	 * Determine whether or not the given point is contained within this shape.
	 *
	 * @param x horizontal coordinate
	 * @param y vertical coordinate
	 * @return true if point is contained
	 */
	boolean contains(int x, int y);

	/**
	 * Determine a <i>bounding box</i> of the current shape. A bounding box is a
	 * box that will fit around the entire shape and, hence, can be used to
	 * determine the maximum width and height of the shape. This is useful when
	 * it comes to drawing the shape!
	 *
	 * @return bounding box of the shape
	 */
	Rectangle boundingBox();

	/**
	 * the resulting shape contains all the points from both shapes
	 *
	 * @param other shape to become union with
	 * @return new shape with a union applied
	 */
	Shape union(Shape other);

	/**
	 * the resulting  shape  contains all
	 * the points that were in both shapes.
	 *
	 * @param other shape to be intersected with
	 * @return new shape with a union intersect
	 */
	Shape intersected(Shape other);

	/**
	 * the  resulting  shape contains all the points that were in the left shape,
	 * but not in the other shape.
	 *
	 * @param other shape to be the right hand side of the difference
	 * @return new shape with a union difference
	 */
	Shape difference(Shape other);
}
