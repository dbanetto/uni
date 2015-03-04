import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.Map;

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

    public void draw(Graphics g, double scale, Location offset) {
        assert(points.length > 0);

        Location prv = null;
        for(Location loc : this.points) {
            if (prv != null) {
                g.drawLine((int)((prv.x - offset.x) * scale), (int)((offset.y - prv.y) * scale),
                           (int)((loc.x - offset.x) * scale), (int)((offset.y - loc.y) * scale)
                );
            }
            prv = loc;
        }
    }

    public static void LoadFromFile(File Segments, Map<Integer, Intersection> intersections, Map<Integer,Road> Roads) {
        assert (Segments.isFile());
        assert (Segments.canRead());
        try {
            BufferedReader segmentsReader = new BufferedReader(new FileReader(Segments));

            segmentsReader.readLine(); // Skip the header line

            String line;
            while ((line = segmentsReader.readLine()) != null) {
                Queue<String> data = new ArrayDeque<>(java.util.Arrays.asList(line.split("\t")));
                int id = Integer.parseInt(data.poll());
                Road parentRoad = Roads.get(id);
                // FIXME: Insert self to RoadID's segments
                double length = Double.parseDouble(data.poll());
                int nodeID1 = Integer.parseInt(data.poll());
                Intersection from = intersections.get(nodeID1); //FIXME: find intersection
                int nodeID2 = Integer.parseInt(data.poll());
                Intersection to = intersections.get(nodeID2); //FIXME: find intersection

                // Make sure there is an even number of lat's and lon's
                assert(data.size() % 2 == 0);
                List<Location> points = new ArrayList<>(data.size() / 2);
                while (data.size() != 0) {
                    double lat = Double.parseDouble(data.poll());
                    double lon = Double.parseDouble(data.poll());

                    points.add(Location.newFromLatLon(lat, lon));
                }
                parentRoad.getSegments().add(new Segment(id, from, to, points.toArray(new Location[points.size()]), length));
            }

        } catch (FileNotFoundException e) {
            System.out.println("Could not find " + Segments.getName() +
                    "\n" + e.toString());
        } catch (IOException e) {
            System.out.println("IO Exception while operating on " + Segments.getName() +
                    "\n" + e.toString());
        }
    }

}
