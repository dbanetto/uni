import java.awt.*;
import java.io.*;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;


public class Intersection implements IDrawable {
    public final Location location;
    public final int id;

    private Set<Road> edges; // FIXME: separate the `from` and `to` edges
    private Rectangle area;
    private Color colour;

    public Intersection(int ID, Location Location) {
        this.id = ID;
        this.location = Location;
        edges = new TreeSet<>(new Comparator<Road>() {
            @Override
            public int compare(Road o1, Road o2) {
                return o1.id - o2.id;
            }
        });
        colour = Color.blue;
        Point pt = location.asPoint(Location.CENTRE, 1.0);
        area = new Rectangle(pt.x, pt.y, 5,5);
    }

    /**
     * @see super.draw
     */
    public void draw(Graphics g, Location originOffset, double scale) {
        g.setColor(this.colour);
        Point pt = location.asPoint(originOffset, scale);
        int scaledSides = Math.min((int)(area.width * (scale * 8.0)), area.width);
        if (scaledSides != 0) {
            g.fillRect(pt.x - (scaledSides / 2), pt.y - (scaledSides / 2), scaledSides, scaledSides);
        }
    }

    @Override
    public boolean equals(Object ob) {
        if (ob instanceof Intersection) {
            Intersection it = (Intersection) (ob);
            return (it.id == this.id);
        }
        return false;
    }

    /**
     * @see super.draw
     */
    @Override
    public Rectangle getArea() {
        return this.area;
    }

    @Override
    public String toString() {
        return "Intersection{" +
                "id=" + id +
                ", location=" + location +
                ", area=" + area +
                '}';
    }

    /**
     *
     * @return A formatted string with the roads that this intersection touches
     */
    public String intersectsWith() {
        String with = "";
        int count = 0;
        Set<Road> segs = new TreeSet<>(new Comparator<Road>() {
            @Override
            public int compare(Road o1, Road o2) {
                return (o1.label.equals(o2.label) ? 0 : 1);
            }
        });
        segs.addAll(edges);
        for (Road r : segs) {
            if (r.label.isEmpty()) { continue; }
            count++;
            if (count == segs.size() && !with.isEmpty()) {
                with += " and " + r.label;
            } else if (with.isEmpty()) {
                with += r.label;
            } else {
                with += ", " + r.label;
            }

        }
        return with;
    }

    /***
     *
     * @param nodes a File pointing to a list of intersections values separated by tabs
     * @param quadMap a QuadMap of Intersections that will be filled while loading
     * @return null on failure, otherwise a map of Intersection IDs to Intersections
     */
    public static java.util.Map<Integer, Intersection> LoadFromFile(File nodes, QuadTree<Intersection> quadMap) {
        TreeMap<Integer, Intersection> intersections = new TreeMap<>(new Comparator<Integer>() {
            @Override
            public int compare(Integer o1, Integer o2) {
                return o1 - o2;
            }
        });

        try {
            BufferedReader reader = new BufferedReader(new FileReader(nodes));
            String line;
            while ((line = reader.readLine()) != null) {
                String[] data = line.split("\t");
                int id     = Integer.parseInt(data[0]);
                double lat = java.lang.Double.parseDouble(data[1]);
                double lon = java.lang.Double.parseDouble(data[2]);

                intersections.put(id, new Intersection(id, Location.newFromLatLon(lat, lon)));
                quadMap.add(intersections.get(id));
            }

        } catch (FileNotFoundException e) {
            System.err.println("Could not find " + nodes.getName() +
                    "\n" + e.toString());
            return null;
        } catch (IOException e) {
            System.err.println("IO Exception while operating on " + nodes.getName() +
                    "\n" + e.toString());
            return null;
        }
        return intersections;
    }

    public Color getColour() {
        return colour;
    }

    public Set<Road> getEdges() {
        return edges;
    }

    public void setColour(Color colour) {
        this.colour = colour;
    }
}
