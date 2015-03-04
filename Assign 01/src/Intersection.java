import java.awt.*;
import java.io.*;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;


public class Intersection {
    public Set<Road> edges;
    public final Location location;
    public final int id;

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
    }

    public void draw(Graphics g, double scale, Location offset) {
        g.setColor(this.colour);
        g.fillOval((int)((this.location.x - offset.x) * scale ),
                   (int)((offset.y - this.location.y) * scale),
                   5, 5);
    }

    @Override
    public boolean equals(Object ob) {
        if (ob instanceof Intersection) {
            Intersection it = (Intersection) (ob);
            return (it.id == this.id);
        }
        return false;
    }

    /***
     *
     * @param nodes a File pointing to a list of intersections values separated by tabs
     * @return null on failure, otherwise an array of Intersections
     */
    public static java.util.Map<Integer, Intersection> LoadFromFile(File nodes) {
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
                double lat = Double.parseDouble(data[1]);
                double lon = Double.parseDouble(data[2]);

                intersections.put(id, new Intersection(id, Location.newFromLatLon(lat, lon)));
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

    public void setColour(Color colour) {
        this.colour = colour;
    }
}
