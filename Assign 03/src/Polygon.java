import java.awt.*;
import java.util.*;

public class Polygon {
    private final Vector3D[] points; // array has order, immutable size and cheap
    private final Color reflective;

    public Polygon(Vector3D[] points, Color reflective) {
        this.points = points;
        this.reflective = reflective;

        // Hard constraints
        if (points.length != 3) {
            throw new IllegalArgumentException("Must have only three points");
        }
    }

    public static Polygon loadFromLine(String line) {

        Queue<String> items = new LinkedList<String>();
        items.addAll(Arrays.asList(line.split(" ")));
        if (items.size() != 12) {
            throw new IllegalArgumentException("Invalid polygon line");
        }

        Vector3D[] points = new Vector3D[3];
        Color reflective;

        for (int i = 0; i < 3; i++) {
            float x, y, z;
            x = Float.parseFloat(items.poll());
            y = Float.parseFloat(items.poll());
            z = Float.parseFloat(items.poll());
            points[i] = new Vector3D(x, y , z);
        }

        int r, g, b;
        r = Integer.parseInt(items.poll());
        g = Integer.parseInt(items.poll());
        b = Integer.parseInt(items.poll());
        reflective = new Color(r, g, b);

        return new Polygon(points, reflective);
    }

    // Getters

    public Vector3D[] getPoints() {
        return points;
    }

    public Color getReflective() {
        return reflective;
    }
}
