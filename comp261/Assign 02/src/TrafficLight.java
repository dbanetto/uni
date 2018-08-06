import java.awt.*;
import java.io.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by drb on 11/04/15.
 */
public class TrafficLight {
    public TrafficLight() {}

    public static Map<Intersection, TrafficLight> loadTrafficLights(File trafficlights, QuadTree<Intersection> intersectionQuadTree) {
        Map<Intersection, TrafficLight> traffic = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(trafficlights));
            String line = reader.readLine(); // skip header line
            while ((line = reader.readLine()) != null) {
                String[] data = line.split("\t");
                double lon     = Double.parseDouble(data[0]);
                double lat     = Double.parseDouble(data[1]);

                Location loc = Location.newFromLatLon(lat, lon);
                Point pt = loc.asPoint(Location.CENTRE, 1.0);
                List<Intersection> itersec = intersectionQuadTree.get(new Rectangle(pt.x - 5,pt.y - 5, 10, 10));
                Intersection found = null;
                for (Intersection i : itersec) {
                    if (i.location.asPoint(Location.CENTRE, 1.0).equals(pt)) {
                        found = i;
                        break;
                    }
                }
                if (found == null && itersec.size() == 1) {
                    found = itersec.get(0);
                }
                if (found != null) {
                    found.setDefaultColour(Color.DARK_GRAY);
                    found.resetColour();
                    traffic.put(found, new TrafficLight());
                } else {
                    System.out.println("Could not find {" + lat + "," + lon + "} out of " + itersec.size() + " found.");
                }
            }

        } catch (FileNotFoundException e) {
            System.err.println("Could not find " + trafficlights.getName() +
                    "\n" + e.toString());
            return null;
        } catch (IOException e) {
            System.err.println("IO Exception while operating on " + trafficlights.getName() +
                    "\n" + e.toString());
            return null;
        }
        return traffic;
    }
}
