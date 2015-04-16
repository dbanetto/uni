import java.awt.*;
import java.util.*;
import java.util.List;

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

    public boolean isHidden() {
        return (points[0].x * points[1].y) >  (points[0].y * points[1].x);
    }

    public static Polygon loadFromLine(String line) {

        Queue<String> items = new LinkedList<>();
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

    public Polygon applyTransformation(Transform transfrom) {

        return null;
    }

    public float getZAt(float x, float y) {

        return 0f;
    }

    public EdgeListItem[] getEdgeList(int imageHeight) {


        float maxyf = Float.MIN_VALUE;
        float minyf = Float.MAX_VALUE;
        for (Vector3D vert: points) {
            if (vert.y > maxyf) {
                maxyf = vert.y;
            }
            if (vert.y < minyf) {
                minyf = vert.y;
            }
        }
        int maxy = Math.round(maxyf);
        int miny = Math.round(minyf);
        EdgeListItem[] list = new EdgeListItem[imageHeight];


        for (int a = 0; a < points.length; a++) {
            int b = (a + 1 >= points.length ? 0 : a + 1);
            Vector3D vertA = (points[a].y < points[b].y ? points[a] : points[b]);
            Vector3D vertB = (points[a].y >= points[b].y ? points[a] : points[b]);

            float mx = (vertB.x-vertA.x) / (vertB.y-vertA.y);
            float mz = (vertB.z-vertA.z) / (vertB.y-vertA.y);

            float x = vertA.x;
            float z = vertA.z;
            int i = Math.round(vertA.y);
            int maxi = Math.round(vertB.y);
            while (i < maxi) {
                if (i >= 0 && i < imageHeight) {
                    EdgeListItem item = list[i];
                    if (item == null) {
                        item = new EdgeListItem();
                        item.put(x, z);
                        list[i] = item;
                    } else {
                        item.put(x, z);
                    }
                }

                x += mx;
                z += mz;
                i++;
            }
            if (maxi > 0 && maxi < imageHeight) {
                EdgeListItem item = list[maxi];
                if (item == null) {
                    item = new EdgeListItem();
                    item.put(x, z);
                    list[maxi] = item;
                } else {
                    item.put(x, z);
                }
            }

        }

        return list;
    }

    // Getters
    public Vector3D[] getPoints() {
        return points;
    }

    public Color getReflective() {
        return reflective;
    }

    public Rectangle getBoudingBox() {
        float maxyf = Float.MIN_VALUE;
        float minyf = Float.MAX_VALUE;
        float maxxf = Float.MIN_VALUE;
        float minxf = Float.MAX_VALUE;
        for (Vector3D vert: points) {
            if (vert.y > maxyf) {
                maxyf = vert.y;
            }
            if (vert.y < minyf) {
                minyf = vert.y;
            }
            if (vert.x > maxxf) {
                maxxf = vert.x;
            }
            if (vert.x < minxf) {
                minxf = vert.x;
            }
        }

        int x = Math.round(minxf), y = Math.round(minyf);
        int bot = Math.round(maxyf), right = Math.round(maxxf);
        return new Rectangle(x, y, right - x, bot - y);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Polygon polygon = (Polygon) o;

        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return (!Arrays.equals(points, polygon.points));

    }

    @Override
    public int hashCode() {
        int result = points != null ? Arrays.hashCode(points) : 0;
        result = 31 * result + (reflective != null ? reflective.hashCode() : 0);
        return result;
    }
}
