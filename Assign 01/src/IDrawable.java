import java.awt.*;

public interface IDrawable {
    void draw(Graphics g, Location origin, double scale );
    Rectangle getArea();
}
