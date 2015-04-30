import java.awt.*;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Created by drb on 15/04/15.
 */
public class LightSource {
    private final Vector3D position;
    private Vector3D intensity = new Vector3D(1f, 1f, 1f); // x = r, y = g, z = b
    private Color incident = Color.white;

    public LightSource(Vector3D position) {
        this.position = position;
    }

    public static LightSource loadFromLine(String line) {
        Queue<String> items = new LinkedList<>();
        items.addAll(Arrays.asList(line.split(" ")));

        // Make sure right size
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
        Vector3D lightPos = this.position.unitVector();

        float cosTheta = lightPos.cosTheta(surfaceUnitNormal);

        // Make sure is positive
        if (cosTheta < 0) {
            cosTheta = 0;
        }
        int r, g, b;

        // Get incident colour
        float ir, ig, ib;
        ir = incident.getRed()   / intensity.x;
        ig = incident.getGreen() / intensity.y;
        ib = incident.getBlue()  / intensity.z;

        // Get reflectance %
        float rr, rg, rb;
        rr = reflectance.getRed()   / 255.0f;
        rg = reflectance.getGreen() / 255.0f;
        rb = reflectance.getBlue()  / 255.0f;

        r = Math.round((ambientLight.getRed()   + cosTheta * ir) * rr);
        g = Math.round((ambientLight.getGreen() + cosTheta * ig) * rg);
        b = Math.round((ambientLight.getBlue()  + cosTheta * ib) * rb);

        // Return bounded 0-255
        return new Color(
                Math.max(Math.min(r,255),0),
                Math.max(Math.min(g,255),0),
                Math.max(Math.min(b,255),0));
    }

    // Getters
    public Vector3D getPosition() {
        return position;
    }

    public Vector3D getIntensity() {
        return intensity;
    }

    public void setIntensity(Vector3D intensity) {
        this.intensity = intensity;
    }

    public Color getIncident() {
        return incident;
    }

    public void setIncident(Color incident) {
        this.incident = incident;
    }
}
