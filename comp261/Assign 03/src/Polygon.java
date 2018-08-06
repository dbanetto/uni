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
        Queue<String> items = new LinkedList<>();
        items.addAll(Arrays.asList(line.split(" ")));

        // Check if right size
        if (items.size() != 12) {
            throw new IllegalArgumentException("Invalid polygon line");
        }

        Vector3D[] points = new Vector3D[3];
        Color reflective;

        // Read 3 vectors
        for (int i = 0; i < 3; i++) {
            float x, y, z;
            x = Float.parseFloat(items.poll());
            y = Float.parseFloat(items.poll());
            z = Float.parseFloat(items.poll());
            points[i] = new Vector3D(x, y , z);
        }

        // Read reflective colour
        int r, g, b;
        r = Integer.parseInt(items.poll());
        g = Integer.parseInt(items.poll());
        b = Integer.parseInt(items.poll());
        reflective = new Color(r, g, b);

        return new Polygon(points, reflective);
    }

    public Polygon applyTransformation(Transform transform) {
        Vector3D[] newPoints = new Vector3D[points.length];
        for (int i = 0; i < points.length; i++) {
            newPoints[i] = transform.multiply(points[i]);
        }
        return new Polygon(newPoints, this.reflective);
    }

    public Vector3D getSurfaceNormal() {
        Vector3D a = points[1].minus(points[0]);
        Vector3D b = points[2].minus(points[1]);

        return a.crossProduct(b);
    }

    public Vector3D getUnitSurfaceNormal() {
        Vector3D a = points[1].minus(points[0]);
        Vector3D b = points[2].minus(points[1]);

        Vector3D n = a.crossProduct(b);
        float sum = Math.abs(n.x) + Math.abs(n.y) + Math.abs(n.z);

        return new Vector3D(n.x / sum, n.y / sum, n.z / sum);
    }

    public EdgeListItem[] getEdgeList(int imageHeight) {

        // Get polygon bounds (in float)
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

        EdgeListItem[] list = new EdgeListItem[imageHeight];

        for (int a = 0; a < points.length; a++) {

            int b = (a + 1 >= points.length ? 0 : a + 1);
            // make sure VertA is to the left of VertB
            Vector3D vertA = (points[a].y < points[b].y ? points[a] : points[b]);
            Vector3D vertB = (points[a].y >= points[b].y ? points[a] : points[b]);
            if (vertA == vertB) { throw new AssertionError(); }

            // Gradients of x and z in respect of y
            float mx = (vertB.x - vertA.x) / (vertB.y - vertA.y);
            float mz = (vertB.z - vertA.z) / (vertB.y - vertA.y);

            // Float sanity checks
            if (Math.abs(vertB.y - vertA.y) < 0.001) {
                mx = 0;
                mz = 0;
            }
            if (Float.isInfinite(mx)) {
                mx = 0;
            } else if (Float.isNaN(mx)) {
                mx = 0;
            }
            if (Float.isInfinite(mz)) {
                mz = 0;
            } else if (Float.isNaN(mz)) {
                mz = 0;
            }

            float x = vertA.x;
            float z = vertA.z;

            int i = Math.round(vertA.y);
            int maxi = Math.round(vertB.y);

            do {
                // make sure in bounds
                if (i >= 0 && i < imageHeight) {
                    EdgeListItem item = list[i];
                    // lazy init
                    if (item == null) {
                        item = new EdgeListItem();
                        item.put(x, z);
                        list[i] = item;
                    } else {
                        item.put(x, z);
                    }
                }

                // step forward in interpolation
                x += mx;
                z += mz;

                // sanity bounds checks
                x = Math.max(Math.min(x, maxxf), minxf);

                i++;
            }  while (i < maxi);

            // make doublely sure last point was added
            if (maxi > 0 && maxi < imageHeight) {
                EdgeListItem item = list[maxi];
                if (item == null) {
                    // lazy init
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

    public Vector3D getMaxVec() {
        float x = Float.MIN_VALUE;
        float y = Float.MIN_VALUE;
        float z = Float.MIN_VALUE;
        for (Vector3D vert: points) {
            if (vert.x > x) {
                x = vert.x;
            }
            if (vert.y > y) {
                y = vert.y;
            }
            if (vert.z > z) {
                z = vert.z;
            }

        }
        return new Vector3D(x,y,z);
    }

    public Vector3D getMinVec() {
        float x = Float.MAX_VALUE;
        float y = Float.MAX_VALUE;
        float z = Float.MAX_VALUE;
        for (Vector3D vert: points) {
            if (vert.x < x) {
                x = vert.x;
            }
            if (vert.y < y) {
                y = vert.y;
            }
            if (vert.z < z) {
                z = vert.z;
            }

        }
        return new Vector3D(x,y,z);
    }

    public Vector3D getCenterVec() {
        float x = 0;
        float y = 0;
        float z = 0;
        for (Vector3D vert: points) {
            x += vert.x;
            y += vert.y;
            z += vert.z;
        }
        return new Vector3D(x/points.length, y/points.length, z/points.length);
    }

    // Getters
    public Vector3D[] getPoints() {
        return points;
    }

    public Color getReflective() {
        return reflective;
    }

    public Rectangle getBondingBox() {
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


    public boolean containsVec(Vector3D vec) {
        for (Vector3D v : this.points) {
            if (vec.equals(v)) {
                return true;
            }
        }
        return false;
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

    @Override
    public String toString() {
        return "Polygon{" +
                "points=" + Arrays.toString(points) +
                ", reflective=" + reflective +
                '}';
    }
}
