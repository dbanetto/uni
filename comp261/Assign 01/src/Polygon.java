import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A representation of the polygon-shapes.mp file
 */
public class Polygon implements IDrawable {

    private static final Map<Integer, Color> typeColours;
    private static final Color MISSING_TYPE = new Color(0xFF00FF);
    static {
        Map<Integer, Color> typeColour = new HashMap<>();

        // Nice looking colours thanks to OpenStreetMap.org
        typeColour.put(0x13, new Color(0xCDCDCD)); // city block

        typeColour.put(0x3c, new Color(0xB5D0D0)); // Lakes, streams and reservoirs
        typeColour.put(0x47, new Color(0xB5D0D0)); // rivers
        typeColour.put(0x3e, new Color(0xB5D0D0)); // ponds / lakes
        typeColour.put(0x40, new Color(0xB5D0D0)); // pond
        typeColour.put(0x41, new Color(0xB5D0D0)); // pond
        typeColour.put(0x42, new Color(0xB5D0D0)); // pond
        typeColour.put(0x48, new Color(0xB5D0D0)); // big river
        typeColour.put(0x28, new Color(0xB5D0D0)); // Ocean

        typeColour.put(0x1a, new Color(0xAACAAE)); // parks
        typeColour.put(0x17, new Color(0xCCF6C9)); // more parks
        typeColour.put(0x18, new Color(0x98B79C)); // golf courses
        typeColour.put(0x16, new Color(0x98B79C)); // reserve
        typeColour.put(0x1e, new Color(0x98B79C)); // trees
        typeColour.put(0x50, new Color(0x98B79C)); // glades
        typeColour.put(0x07, new Color(0xE8E6E1)); // Airport grass
        typeColour.put(0x19, new Color(0x33CC99)); // Stadium

        typeColour.put(0x02, new Color(0xE1E1E1)); // suburbs
        typeColour.put(0x08, new Color(0xCBDCBD)); // CBD
        typeColour.put(0x0b, new Color(0xD9D0C9)); // Hospital

        typeColour.put(0x0e, new Color(0xBBBBCC)); // airstrip

        typeColour.put(0x0a, new Color(0xD9D0C9)); // school
        typeColour.put(0x45, new Color(0xD9D0C9)); // oxidation pond

        typeColour.put(0x05, new Color(0xFF0000)); // what is 5?
        typeColours = typeColour;
    }

    private int type;
    private int endLevel;
    private int cityIdx;
    private Location[] locations;
    Rectangle area;

    public Polygon(int Type, int EndLevel, int CityIdx, Location[] Locations) {
        this.type = Type;
        this.endLevel = EndLevel;
        this.cityIdx = CityIdx;
        this.locations = Locations;

        // Calculate rectangular area
        Point topLeft = new Point(Integer.MAX_VALUE,Integer.MAX_VALUE);
        Point botRight = new Point(Integer.MIN_VALUE,Integer.MIN_VALUE);
        for (Location loc : locations) {
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

    /**
     * @see super.draw
     */
    @Override
    public void draw(Graphics g, Location originOffset, double scale) {
        if (typeColours.containsKey(this.type)) {
            g.setColor(typeColours.get(this.type));
        } else {
            g.setColor(MISSING_TYPE);
            System.out.println("Defaulting colour for type:" + this.type);
        }
        int[] x = new int[locations.length];
        int[] y = new int[locations.length];
        int i = 0;
        for (Location loc: locations) {
            Point pt = loc.asPoint(originOffset, scale);
            x[i] = pt.x;
            y[i] = pt.y;
            i++;
        }
        g.fillPolygon(x, y, i);
    }

    /**
     * @see super.getArea
     */
    @Override
    public Rectangle getArea() {
        return area;
    }

    /**
     *
     * @param file to be read from
     * @param polygonQuadTree a QuadTree to be filled while loading
     */
    public static void loadPolygons(File file, QuadTree<Polygon> polygonQuadTree) {
        assert (file.isFile());
        assert (file.canRead());

        try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;
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
                    polygonQuadTree.add(new Polygon(type, endLevel, cityIdx,
                            locations.toArray(new Location[locations.size()])
                    ));
                }

                // Data Fields
                if (line.startsWith("Type=")) {
                    String data = line.substring("Type=".length()).trim();
                    type = Integer.parseInt(data.substring("0x".length()), 16);

                } else if (line.startsWith("EndLevel=")) {
                    String data = line.substring("EndLevel=".length()).trim();
                    endLevel = Integer.parseInt(data, 10);

                } else if (line.startsWith("CityIdx=")) {
                    String data = line.substring("CityIdx=".length()).trim();
                    cityIdx = Integer.parseInt(data, 10);

                } else if (line.startsWith("Data0=")) {
                    // Handle multiple Date0 entries with making multiple polygons of the same base data
                    if (locations.size() != 0) {
                        polygonQuadTree.add(new Polygon(type, endLevel, cityIdx,
                                locations.toArray(new Location[locations.size()])
                        ));
                        locations.clear();
                    }

                    // Split up comma separated tuple
                    // parse coordinates
                    String data = line.substring("Data0=".length()).trim();
                    // Tries to split up all tuples, but leaves '(' in the first element and ')' in the last element
                    String[] locs = data.split("\\),[\\(?]");
                    // remove first (
                    locs[0] = locs[0].substring(1);
                    // remove last )
                    locs[locs.length - 1] = locs[locs.length - 1].substring(0, locs[locs.length - 1].length() - 1);
                    for (String loc : locs) {
                        String[] latlon = loc.split(",");
                        double lat = Double.parseDouble(latlon[0]);
                        double lon = Double.parseDouble(latlon[1]);

                        locations.add(Location.newFromLatLon(lat, lon));
                    }
                }
            }
        } catch (FileNotFoundException ex) {
            System.err.println("Could not find " + file.getName() +
                    "\n" + ex.toString());
        } catch (IOException ex) {
        System.err.println("IOException thrown when reading " + file.getName() +
                "\n" + ex.toString());
         }
    }
}
