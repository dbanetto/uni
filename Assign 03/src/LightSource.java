import java.awt.*;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Created by drb on 15/04/15.
 */
public class LightSource {
    private final Vector3D position;
    private Vector3D intensity = new Vector3D(0.5f, 0.5f, 0.5f);
    private Color incident = Color.white;

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

    public Color computeIllumination(Vector3D surfaceUnitNormal, Color ambientLight, Color reflectance) {
        float cosTheta = this.position.cosTheta(surfaceUnitNormal);
        int r, g, b;

        float ir, ig, ib;
        ir = incident.getRed() / intensity.x;
        ig = incident.getGreen() / intensity.y;
        ib = incident.getBlue()  / intensity.z;

        float rr, rg, rb;
        rr = reflectance.getRed()   / 255.0f;
        rg = reflectance.getGreen() / 255.0f;
        rb = reflectance.getBlue()  / 255.0f;

        r = (int)Math.max(Math.min((ambientLight.getRed() + (cosTheta * ir)) * rr, 255.0f), 0.0f);
        g = (int)Math.max(Math.min((ambientLight.getGreen() + (cosTheta * ig)) * rg, 255.0f), 0.0f);
        b = (int)Math.max(Math.min((ambientLight.getBlue() + (cosTheta * ib)) * rb, 255.0f), 0.0f);


        return new Color(r, g, b);
    }

    // Getters
    public Vector3D getPosition() {
        return position;
    }
}
