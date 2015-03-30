import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.Queue;

public class Road implements IDrawable {
    int id;
    int type;
    String label;
    String city;
    boolean isOneWay;
    byte speedLimit;
    // TODO: Make enum for Road Class
    byte roadClass;
    // TODO: Make flags for notFor* to compress to a byte
    boolean notForCar;
    boolean notForPedestrians;
    boolean notForBicycle;
    double length;

    Color colour;

    private Set<RoadSegment> roadSegments;

    public Road(int ID, int Type, String Label, String City, boolean IsOneWay, byte SpeedLimit, byte RoadClass,
                boolean NotForCars, boolean NotForPedestrians, boolean NotForBicycles) {
        this.id = ID;
        this.type = Type;

        this.label = Label;
        this.city = City;
        this.isOneWay = IsOneWay;
        this.speedLimit = SpeedLimit;
        this.roadClass = RoadClass;
        this.notForCar = NotForCars;
        this.notForPedestrians = NotForPedestrians;
        this.notForBicycle = NotForBicycles;

        this.length = 0.0;
        this.roadSegments = new HashSet<>();
        colour = Color.black;
    }

    /**
     * @see super.draw
     */
    public void draw(Graphics g, Location originOffset, double scale) {
        if (this.roadSegments == null) return;

        g.setColor(colour);
        for(RoadSegment seg : this.roadSegments) {
            seg.draw(g, originOffset, scale);
        }
    }

    @Override
    public boolean equals(Object ob) {
        if (ob instanceof Road) {
            Road rd = (Road)(ob);
            return (rd.id == this.id);
        }
        return false;
    }

    /**
     * @see super.getArea
     */
    @Override
    public Rectangle getArea() {
        return null;
    }

    /**
     * Load Roads from tabulated file
     * @param Roads file to be loaded from
     * @param RoadTrie Trie of Road names to be filled
     * @param RoadLabel a map of road names to list of roads with that name
     * @return A map of Road ID's to Roads
     */
    public static java.util.Map<Integer, Road> LoadFromFile(File Roads, TrieNode RoadTrie, Map<String, List<Road>> RoadLabel) {
        TreeMap<Integer, Road> roads = new TreeMap<>();
        assert (Roads.isFile());
        assert (Roads.canRead());

        try {
            BufferedReader roadsReader = new BufferedReader(new FileReader(Roads));

            roadsReader.readLine(); // Skip the header line

            String line;
            while ((line = roadsReader.readLine()) != null) {
                Queue<String> data = new ArrayDeque<>(java.util.Arrays.asList(line.split("\t")));
                int id = Integer.parseInt(data.poll());
                int type = Integer.parseInt(data.poll());
                String label = data.poll().trim();
                RoadTrie.insert(label);
                String city = data.poll().trim();
                boolean oneway = Integer.parseInt(data.poll()) == 1;
                byte speed = Byte.parseByte(data.poll());
                byte roadclass = Byte.parseByte(data.poll());
                boolean notforcar = Integer.parseInt(data.poll()) == 1;
                boolean notforpede = Integer.parseInt(data.poll()) == 1;
                boolean notforbicy = Integer.parseInt(data.poll()) == 1;

                roads.put(id, new Road(id, type, label, city, oneway, speed, roadclass, notforcar, notforpede, notforbicy));
                if (!RoadLabel.containsKey(label)) {
                    RoadLabel.put(label, new ArrayList<Road>());
                }
                RoadLabel.get(label).add(roads.get(id));
            }

        } catch (FileNotFoundException e) {
            System.out.println("Could not find " + Roads.getName() +
                    "\n" + e.toString());
            return null;
        } catch (IOException e) {
            System.out.println("IO Exception while operating on " + Roads.getName() +
                    "\n" + e.toString());
            return null;
        }

        return roads;
    }
    // Getters and Setters
    public Set<RoadSegment> getRoadSegments() {
        return roadSegments;
    }

    public void setRoadSegments(Set<RoadSegment> roadSegments) {
        this.roadSegments = roadSegments;
    }
    public Color getColour() {
        return colour;
    }

    public void setColour(Color colour) {
        this.colour = colour;
    }

    public int getId() {
        return id;
    }

    public int getType() {
        return type;
    }

    public String getLabel() {
        return label;
    }

    public String getCity() {
        return city;
    }

    public boolean isOneWay() {
        return isOneWay;
    }

    public byte getSpeedLimit() {
        return speedLimit;
    }

    public byte getRoadClass() {
        return roadClass;
    }

    public boolean isNotForCar() {
        return notForCar;
    }

    public boolean isNotForPedestrians() {
        return notForPedestrians;
    }

    public boolean isNotForBicycle() {
        return notForBicycle;
    }

    public double getLength() {
        return length;
    }
}
