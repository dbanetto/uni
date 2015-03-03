import java.io.*;
import java.util.*;

/**
 * Created by drb on 03/03/15.
 */
public class Segment {
    int roadID;
    Intersection from;
    Intersection to;
    double length;
    Location[] points; // TODO: Better name

    public Segment (int RoadID, Intersection From, Intersection To, Location[] Points, double Length) {
        this.roadID = RoadID;
        this.from = From;
        this.to = To;
        this.length = Length;
        this.points = Points;
    }

    public static Set<Segment> LoadFromFile(File Segments) {
        assert (Segments.isFile());
        assert (Segments.canRead());
        Set<Segment> segments = new HashSet<>();
        try {
            BufferedReader segmentsReader = new BufferedReader(new FileReader(Segments));

            segmentsReader.readLine(); // Skip the header line

            String line;
            while ((line = segmentsReader.readLine()) != null) {
                Queue<String> data = new ArrayDeque<>(java.util.Arrays.asList(line.split("\t")));
                int id = Integer.parseInt(data.poll());
                // FIXME: Insert self to RoadID's segments
                double length = Double.parseDouble(data.poll());
                int nodeID1 = Integer.parseInt(data.poll());
                Intersection from = null; //FIXME: find intersection
                int nodeID2 = Integer.parseInt(data.poll());
                Intersection to = null; //FIXME: find intersection

                // Make sure there is an even number of lat's and lon's
                assert(data.size() % 2 == 0);
                List<Location> points = new ArrayList<>(data.size() / 2);
                while (data.size() != 0) {
                    double lat = Double.parseDouble(data.poll());
                    double lon = Double.parseDouble(data.poll());

                    points.add(Location.newFromLatLon(lat, lon));
                }
                if (!segments.add(new Segment(id, from, to, points.toArray(new Location[points.size()]), length))) {
                    System.out.println("Failed to insert");
                }
            }

        } catch (FileNotFoundException e) {
            System.out.println("Could not find " + Segments.getName() +
                    "\n" + e.toString());
            return null;
        } catch (IOException e) {
            System.out.println("IO Exception while operating on " + Segments.getName() +
                    "\n" + e.toString());
            return null;
        }
        return segments;
    }

}
