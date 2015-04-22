import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.*;
import java.util.List;


public class Scene {
    Collection<Polygon> polygons;
    List<LightSource> lights;
    Color background = Color.white;

    public Scene(Collection<Polygon> polygons, List<LightSource> lights) {
        this.polygons = polygons;
        this.lights = lights;
    }

    public static Scene loadFromFile(File file) {
        List<Polygon> polygons = new ArrayList<>(); // FIXME: Get a better data structure
        List<LightSource> lights = new ArrayList<>();

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

    public BufferedImage render(Rectangle imageBounds, Camera camera, Color ambientLight) {
        float[][] depthBuffer = new float[imageBounds.width][imageBounds.height];
        Color[][] colourBuffer = new Color[imageBounds.width][imageBounds.height];

        Transform transform = camera.getTransformation();
        for (Polygon normalPoly: this.polygons) {

            Polygon transPoly = normalPoly.applyTransformation(transform);
            Rectangle polyBounds = transPoly.getBondingBox();

            // clipping
            if (imageBounds.intersects(polyBounds)) {

                // Check if looking polygon the right way
                Vector3D surfaceTransNormal = transPoly.getSurfaceNormal();
                if (surfaceTransNormal.z < 0) {
                    EdgeListItem[] EL = transPoly.getEdgeList(imageBounds.height);
                    for (int y = 0; y < EL.length - 1; y++) {
                        if (EL[y] == null) { continue; }

                        int x = Math.round(EL[y].getX_left());
                        float z = EL[y].getZ_left();

                        float mz = (EL[y].getZ_right() - EL[y].getZ_left()) / (EL[y].getX_right() - EL[y].getX_left());
                        if (Math.abs(EL[y].getX_right() - EL[y].getX_left()) < 0.001) {
                            mz = 0;
                        }

                        // Iterate through horz line
                        while (x <= Math.round(EL[y].getX_right())) {
                            if (x < 0 ||
                                x >= imageBounds.width ||
                                y < 0 && y >= imageBounds.height ||
                                !polyBounds.contains(x, y)) {
                                z += mz;
                                x++;
                                continue;
                            }

                            // Apply shading
                            Color pixel;
                            int r = 0, g = 0, b = 0;
                            for (LightSource light : lights) {
                                Color l = light.computeIllumination(
                                        transform,
                                        normalPoly.getSurfaceNormal().unitVector(),
                                        ambientLight,
                                        transPoly.getReflective());

                                // Additive merge
                                r = Math.min(l.getRed() +  r, 255);
                                g = Math.min(l.getGreen() +  g, 255);
                                b = Math.min(l.getBlue() +  b, 255);
                            }
                            pixel = new Color(r, g, b);

                            // Apply to z-buffer
                            if (colourBuffer[x][y] != null) {
                                if (z < depthBuffer[x][y] ) {
                                    depthBuffer[x][y] = z;
                                    colourBuffer[x][y] = pixel;
                                }
                            } else {
                                depthBuffer[x][y] = z;
                                colourBuffer[x][y] = pixel;
                            }
                            z += mz;
                            x++;
                        }
                    }
                }
            }
        }

        BufferedImage img = new BufferedImage(imageBounds.width, imageBounds.height, BufferedImage.TYPE_INT_RGB);
        for (int x = 0; x < colourBuffer.length; x++) {
            for (int y = 0; y < colourBuffer[0].length; y++) {
                if (colourBuffer[x][y] != null) {
                    img.setRGB(x, y, colourBuffer[x][y].getRGB());
                } else {
                    img.setRGB(x, y, background.getRGB());
                }
            }
        }

        return img;
    }
}
