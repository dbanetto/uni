import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.Queue;

public class RoadSegment implements IDrawable {
    Road parent;
    Intersection from;
    Intersection to;
    double length;
    Location[] points; // TODO: Better name
    Rectangle area;

    public RoadSegment(Road parent, Intersection from, Intersection to, Location[] points, double length) {
        this.parent = parent;
        this.from = from;
        this.to = to;
        this.length = length;
        this.points = points;

        // Calculate rectangular area
        Point topLeft = new Point(Integer.MAX_VALUE,Integer.MAX_VALUE);
        Point botRight = new Point(Integer.MIN_VALUE,Integer.MIN_VALUE);
        for (Location loc : points) {
            Point pt = loc.asPoint(Location.CENTRE, 1.0);
            if (pt.x < topLeft.x) {
                topLeft.x = pt.x;
            }
            if (pt.x > botRight.x) {
                botRight.x = pt.x;
            }
            if (pt.y < topLeft.y) {
                topLeft.y = pt.y;
            }
            if (pt.y > botRight.y) {
                botRight.y = pt.y;
            }
        }
        this.area = new Rectangle(topLeft.x, topLeft.y, botRight.x - topLeft.x + 1, botRight.y - topLeft.y + 1);
    }

    public void draw(Graphics g, Location originOffset,  double scale) {
        assert(points.length > 0);

        Point prvpt = null; // previous point
        for(Location loc : this.points) {
            Point pt = loc.asPoint(originOffset, scale);
            if (prvpt != null) {
                g.drawLine(pt.x, pt.y,
                           prvpt.x, prvpt.y);
            }
            prvpt = pt;
        }
    }

    @Override
    public Rectangle getArea() {
        return area;
    }

    /**
     *
     * @param segments Formatted file
     * @param intersections  Intersection to assoicate with
     * @param roads
     * @param roadSegmentQuadTree
     */
    public static void LoadFromFile(File segments, Map<Integer, Intersection> intersections, Map<Integer,Road> roads, QuadTree<RoadSegment> roadSegmentQuadTree) {
        assert (segments.isFile());
        assert (segments.canRead());
        try {
            BufferedReader segmentsReader = new BufferedReader(new FileReader(segments));

            segmentsReader.readLine(); // Skip the header line

            String line;
            while ((line = segmentsReader.readLine()) != null) {
                Queue<String> data = new ArrayDeque<>(java.util.Arrays.asList(line.split("\t")));

                int id = Integer.parseInt(data.poll());
                Road parentRoad = roads.get(id);

                double length = Double.parseDouble(data.poll());
                // TODO: Handle one way roads
                int nodeID1 = Integer.parseInt(data.poll());
                Intersection from = intersections.get(nodeID1);
                from.getEdges().add(parentRoad);

                int nodeID2 = Integer.parseInt(data.poll());
                Intersection to = intersections.get(nodeID2);
                to.getEdges().add(parentRoad);


                // Make sure there is an even number of lat's and lon's
                assert(data.size() % 2 == 0);
                List<Location> points = new ArrayList<>(data.size() / 2);
                while (data.size() != 0) {
                    double lat = Double.parseDouble(data.poll());
                    double lon = Double.parseDouble(data.poll());

                    points.add(Location.newFromLatLon(lat, lon));
                }
                RoadSegment seg = new RoadSegment(parentRoad, from, to, points.toArray(new Location[points.size()]), length);
                parentRoad.getRoadSegments().add(seg);
                roadSegmentQuadTree.add(seg);
            }

        } catch (FileNotFoundException e) {
            System.out.println("Could not find " + segments.getName() +
                    "\n" + e.toString());
        } catch (IOException e) {
            System.out.println("IO Exception while operating on " + segments.getName() +
                    "\n" + e.toString());
        }
    }

}
