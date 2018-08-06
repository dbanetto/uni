import java.awt.*;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Created by drb on 15/04/15.
 */
public class LightSource {
    private final Vector3D direction;
    private Vector3D intensity = new Vector3D(1f, 1f, 1f); // x = r, y = g, z = b
    private Color incident = Color.white;

    public LightSource(Vector3D direction) {
        this.direction = direction;
    }

    public static LightSource loadFromLine(String line) {
        Queue<String> items = new LinkedList<>();
        items.addAll(Arrays.asList(line.split(" ")));

        // Make sure right size
        if (items.size() != 3) {
            throw new IllegalArgumentException("Invalid light source line");
        }

        Vector3D direction;
        // read in the direction pointed to
        float x, y, z;
        x = Float.parseFloat(items.poll());
        y = Float.parseFloat(items.poll());
        z = Float.parseFloat(items.poll());
        direction = new Vector3D(x, y , z);

        return new LightSource(direction);
    }

    public Color computeIllumination(Vector3D surfaceUnitNormal, Color ambientLight, Color reflectance) {
        Vector3D lightPos = this.direction.unitVector();

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

        r = Math.round((ambientLight.getRed() + cosTheta * ir) * rr);
        g = Math.round((ambientLight.getGreen() + cosTheta * ig) * rg);
        b = Math.round((ambientLight.getBlue()  + cosTheta * ib) * rb);

        // Return bounded 0-255
        return new Color(
                Math.max(Math.min(r,255),0),
                Math.max(Math.min(g,255),0),
                Math.max(Math.min(b,255),0));
    }

    public Color computeIlluminationGouraud(Color[] vertexColours, Vector3D[] points, Vector3D position, Color ambientLight, Color reflectance) {
        Vector3D vertA_top = null,  vertA_bot= null;
        Color colourA_top = null, colourA_bot = null;

        Vector3D vertB_top= null,  vertB_bot= null;
        Color colourB_top = null, colourB_bot = null;

        for (int a = 0; a < points.length; a++) {
            int b = (a + 1 >= points.length ? 0 : a + 1);
            // make sure VertA is to the left of VertB
            int va = (points[a].y < points[b].y ? a : b);
            int vb = (points[a].y >= points[b].y ? a : b);
            Vector3D vertA = points[va];
            Vector3D vertB = points[vb];

            if (vertA.y < position.y && position.y <= vertB.y) {
                if (vertA_top == null) {
                    vertA_top = vertA;
                    vertA_bot = vertB;
                    colourA_top = vertexColours[va];
                    colourA_bot = vertexColours[vb];
                } else {
                    vertB_top = vertA;
                    vertB_bot = vertB;
                    colourB_top = vertexColours[va];
                    colourB_bot = vertexColours[vb];
                    break;
                }
            }
        }
        if (vertA_top == null) {
            vertA_top = points[0];
            vertA_bot =  points[1];
            vertB_top = points[0];
            vertB_bot =  points[1];
            colourA_top = vertexColours[0];
            colourA_bot = vertexColours[1];
            colourB_top = vertexColours[0];
            colourB_bot = vertexColours[1];
        }

        // Solve for left side
        float a_x = ((vertA_bot.x - vertA_top.x) / (vertA_bot.y - vertA_top.y)) * position.y + vertA_bot.x;
        float a_z = ((vertA_bot.z - vertA_top.z) / (vertA_bot.y - vertA_top.y)) * position.y + vertA_bot.z;

        float a_r = ((colourA_bot.getRed()   - colourA_top.getRed())   / (vertA_bot.y - vertA_top.y)) * position.y + colourA_bot.getRed();
        float a_g = ((colourA_bot.getGreen() - colourA_top.getGreen()) / (vertA_bot.y - vertA_top.y)) * position.y + colourA_bot.getGreen();
        float a_b = ((colourA_bot.getBlue()  - colourA_top.getBlue())  / (vertA_bot.y - vertA_top.y)) * position.y + colourA_bot.getBlue();

        // Solve for right side
        float b_x = ((vertB_bot.x - vertB_top.x) / (vertB_bot.y - vertB_top.y)) * position.y + vertB_bot.x;
        float b_z = ((vertB_bot.z - vertB_top.z) / (vertB_bot.y - vertB_top.y)) * position.y + vertB_bot.z;

        float b_r = ((colourB_bot.getRed()   - colourB_top.getRed())   / (vertB_bot.y - vertB_top.y)) * position.y + colourB_bot.getRed();
        float b_g = ((colourB_bot.getGreen() - colourB_top.getGreen()) / (vertB_bot.y - vertB_top.y)) * position.y + colourB_bot.getGreen();
        float b_b = ((colourB_bot.getBlue()  - colourB_top.getBlue())  / (vertB_bot.y - vertB_top.y)) * position.y + colourB_bot.getBlue();



        float cr = ((a_r - b_r) / (a_x - b_x) * position.x + a_r);
        float cg = ((a_g - b_g) / (a_x - b_x) * position.x + a_g);
        float cb = ((a_b - b_b) / (a_x - b_x) * position.x + a_b);

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

        int r = Math.round((ambientLight.getRed() + (cr * ir)) * rr);
        int g = Math.round((ambientLight.getGreen() +(cg * ig)) * rg);
        int b = Math.round((ambientLight.getBlue()  + (cb * ib)) * rb);

        // Return bounded 0-255
        return new Color(
                Math.max(Math.min(r,255),0),
                Math.max(Math.min(g,255),0),
                Math.max(Math.min(b,255),0));
    }

    // Getters
    public Vector3D getDirection() {
        return direction;
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
