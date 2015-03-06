import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.Map;

/**
 * Created by drb on 03/03/15.
 */
public class Segment implements IDrawable {
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

    public void draw(Graphics g, Location origin,  double scale) {
        assert(points.length > 0);

        Location prv = null;
        Point prvpt = null;
        for(Location loc : this.points) {
            Point pt = loc.asPoint(origin, scale);
            if (prv != null) {
                g.drawLine(pt.x, pt.y,
                           prvpt.x, prvpt.y);
            }
            prv = loc;
            prvpt = pt;
        }
    }

    @Override
    public Rectangle getArea() {
        // TODO: Find area of the points (make rect from top-left and bot-right)
        return null;
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
