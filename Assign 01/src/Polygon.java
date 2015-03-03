import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * TODO: Write JavaDoc
 */
public class Polygon {

    private int type;
    private int endLevel;
    private int cityIdx;
    private Location[] locations;

    public Polygon(int Type, int EndLevel, int CityIdx, Location[] Locations) {
        this.type = Type;
        this.endLevel = EndLevel;
        this.cityIdx = CityIdx;
        this.locations = Locations;
    }

    public static Polygon[] parse(File file) {
        assert (file.isFile());
        assert (file.canRead());

        ArrayList<Polygon> polygons = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;
            Boolean inPolygon = false;
            int type = 0, endLevel = 0, cityIdx = 0;
            List<Location> locations = new ArrayList<>();
            while ((line = reader.readLine()) != null) {
                // Directives
                if (line.equals("[POLYGON]")) {
                    // clean out data
                    type = 0;
                    endLevel = 0;
                    cityIdx = 0;
                    locations = new ArrayList<>();
                } else if (line.equals("[END]")) {
                    // Create polygon
                    polygons.add(new Polygon(type, endLevel, cityIdx,
                            (Location[])(locations.toArray())
                            ));
                }

                // Data Fields
                if (line.startsWith("Type=")) {
                    String data = line.substring("Type=".length());
                    type = Integer.parseInt(data, 16);
                } else if (line.startsWith("EndLevel=")) {
                    String data = line.substring("EndLevel=".length());
                    endLevel = Integer.parseInt(data, 10);
                } else if (line.startsWith("CityIdx=")) {
                    String data = line.substring("CityIdx=".length());
                    cityIdx = Integer.parseInt(data, 10);
                } else if (line.startsWith("Data0=")) {
                    // Split up comma separated tuple
                    // parse coordinates
                }
            }
        } catch (FileNotFoundException ex) {
            System.err.println("Could not find " + file.getName() +
                    "\n" + ex.toString());
        } catch (IOException ex) {
        System.err.println("IOException thrown when reading " + file.getName() +
                "\n" + ex.toString());
         }
        return (Polygon[])(polygons.toArray());
    }
}
