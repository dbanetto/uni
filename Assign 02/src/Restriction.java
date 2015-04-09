import java.io.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by drb on 07/04/15.
 */
public class Restriction {
    private Map<Integer, RestrictionTuple> restriction;

    private Restriction() {
        restriction = new HashMap<>();
    }

    public boolean isRestricted(Intersection from, RoadSegment from_via, Intersection at, RoadSegment using, Intersection to) {
        if (restriction.containsKey(at.id)) {
            return restriction.get(at.id).equals(new RestrictionTuple(from.id, from_via.parent.id, at.id, using.parent.id, to.id));
        }
        return false;
    }

    public static Restriction loadRestriction(File file) {
        Restriction re = new Restriction();

        try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;
            while ((line = reader.readLine()) != null) {
                String[] data = line.split("\t");
                int a_id     = Integer.parseInt(data[0]);
                int road_a2b_id     = Integer.parseInt(data[1]);
                int b_id     = Integer.parseInt(data[2]);
                int road_b2c_id     = Integer.parseInt(data[3]);
                int c_id     = Integer.parseInt(data[4]);

                re.restriction.put(b_id, new RestrictionTuple(a_id, road_a2b_id, b_id, road_b2c_id, c_id));
            }

        } catch (FileNotFoundException e) {
            System.err.println("Could not find " + file.getName() +
                    "\n" + e.toString());
            return null;
        } catch (IOException e) {
            System.err.println("IO Exception while operating on " + file.getName() +
                    "\n" + e.toString());
            return null;
        }

        return re;
    }

    private static class RestrictionTuple {

        public RestrictionTuple(int a_id, int a_road_id, int b_id, int b_road_id, int c_id) {
            this.a_id = a_id;
            this.a_road_id = a_road_id;
            this.b_id = b_id;
            this.b_road_id = b_road_id;
            this.c_id = c_id;
        }

        int a_id;
        int a_road_id;
        int b_id;
        int b_road_id;
        int c_id;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            RestrictionTuple that = (RestrictionTuple) o;

            if (a_id != that.a_id) return false;
            if (a_road_id != that.a_road_id) return false;
            if (b_id != that.b_id) return false;
            if (b_road_id != that.b_road_id) return false;
            return c_id == that.c_id;

        }

        @Override
        public int hashCode() {
            int result = a_id;
            result = 31 * result + a_road_id;
            result = 31 * result + b_id;
            result = 31 * result + b_road_id;
            result = 31 * result + c_id;
            return result;
        }
    }
}
