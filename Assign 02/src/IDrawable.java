import java.awt.*;

public interface IDrawable {
    /**
     * Render the graphics panel with offset and scale
     *
     * @param g the graphics to be drawn to
     * @param originOffset the location of the screen
     * @param scale the scale the screen is rendered at
     */
    void draw(Graphics g, Location originOffset, double scale );

    /**
     *
     * @return Area of the drawable
     */
    Rectangle getArea();
}
