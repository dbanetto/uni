import java.io.*;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;


public class Intersection {
    public Set<Road> edges;
    public final Location location;
    public final int id;

    public Intersection(int ID, Location Location) {
        this.id = ID;
        this.location = Location;
        edges = new TreeSet<>(new Comparator<Road>() {
            @Override
            public int compare(Road o1, Road o2) {
                return o1.id - o2.id;
            }
        });
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
    public static Set<Intersection> LoadFromFile(File nodes) {
        Set<Intersection> intersections = new TreeSet<>(new Comparator<Intersection>() {
            @Override
            public int compare(Intersection o1, Intersection o2) {
                return o1.id - o2.id;
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

                intersections.add(new Intersection(id, Location.newFromLatLon(lat, lon)));
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
}
