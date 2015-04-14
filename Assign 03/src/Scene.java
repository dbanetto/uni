import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by drb on 15/04/15.
 */
public class Scene {
    Collection<Polygon> polygons;
    List<LightSource> lights;

    public Scene(Collection<Polygon> polygons, List<LightSource> lights) {
        this.polygons = polygons;
        this.lights = lights;
    }

    public static Scene loadFromFile(File file) {
        List<Polygon> polygons = new ArrayList<Polygon>(); // FIXME: Get a better data structure
        List<LightSource> lights = new ArrayList<LightSource>();

        try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;

            while ((line = reader.readLine()) != null) {
                switch (line.split(" ").length) {
                    case 3:
                        lights.add(LightSource.loadFromLine(line));
                        break;
                    case 12:
                        polygons.add(Polygon.loadFromLine(line));
                        break;
                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("Could not find " + file.getName() +
                    "\n" + e.toString());
            return null;
        } catch (IOException e) {
            System.out.println("IO Exception while operating on " + file.getName() +
                    "\n" + e.toString());
            return null;
        }

        return new Scene(polygons, lights);
    }
}
