import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Created by drb on 15/04/15.
 */
public class LightSource {
    private final Vector3D position;

    public LightSource(Vector3D position) {
        this.position = position;
    }

    public static LightSource loadFromLine(String line) {
        Queue<String> items = new LinkedList<String>();
        items.addAll(Arrays.asList(line.split(" ")));

        if (items.size() != 3) {
            throw new IllegalArgumentException("Invalid light source line");
        }

        Vector3D position;

        float x, y, z;
        x = Float.parseFloat(items.poll());
        y = Float.parseFloat(items.poll());
        z = Float.parseFloat(items.poll());
        position = new Vector3D(x, y , z);

        return new LightSource(position);
    }

    // Getters
    public Vector3D getPosition() {
        return position;
    }
}
