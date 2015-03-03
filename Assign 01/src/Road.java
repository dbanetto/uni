import java.io.*;
import java.util.*;

public class Road {
    int id;
    int type;
    String label;
    String city;
    boolean isOneWay;
    byte speedLimit;
    // TODO: Make enum for Road Class
    byte roadClass;
    // TODO: Make flags for notFor*
    boolean notForCar;
    boolean notForPedestrians;
    boolean notForBicycle;
    double length;

    private Set<Location> segments;

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
        this.segments = null;
    }

    public Set<Location> getSegments() {
        return segments;
    }

    public void setSegments(Set<Location> segments) {
        this.segments = segments;
    }

    @Override
    public boolean equals(Object ob) {
        if (ob instanceof Road) {
            Road rd = (Road)(ob);
            return (rd.id == this.id);
        }
        return false;
    }

    public static java.util.Map<Integer, Road> LoadFromFile(File Roads) {
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
                String label = data.poll();
                String city = data.poll();
                boolean oneway = Integer.parseInt(data.poll()) == 1;
                byte speed = Byte.parseByte(data.poll());
                byte roadclass = Byte.parseByte(data.poll());
                boolean notforcar = Integer.parseInt(data.poll()) == 1;
                boolean notforpede = Integer.parseInt(data.poll()) == 1;
                boolean notforbicy = Integer.parseInt(data.poll()) == 1;

               roads.put(id, new Road(id, type, label, city, oneway, speed, roadclass, notforcar, notforpede, notforbicy));
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
}
