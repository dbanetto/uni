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

    public BufferedImage render(Rectangle imageBounds, Vector3D cameraPosition, Color ambientLight) {
        Float[][] depthBuffer = new Float[imageBounds.width][imageBounds.height];
        Color[][] colourBuffer = new Color[imageBounds.width][imageBounds.height];

        // Transform polygons

        Set<Polygon> hidden = new HashSet<>();
        for (Polygon poly: this.polygons) {
            if (imageBounds.intersects(poly.getBoudingBox())) {
                System.out.println(poly);
                // hidden.add(poly);

                EdgeListItem[] EL = poly.getEdgeList(imageBounds.height);
                for (int y = 0; y < EL.length - 1; y++) {
                    if (EL[y] == null) { continue; }

                    int x = Math.round(EL[y].getX_left());
                    float z = EL[y].getZ_left();

                    float mz = (EL[y].getZ_right() - EL[y].getZ_left()) / (EL[y].getX_right() - EL[y].getX_left());
                    while (x <= Math.round(EL[y].getX_right())) {
                        if (x < 0 || x >= imageBounds.width || y < 0 && y >= imageBounds.height) {
                            z += mz;
                            x++;
                            continue;
                        }
                        Float depth = depthBuffer[x][y];
                        if (depth != null) {
                            if (z < depthBuffer[x][y] ) {
                                depthBuffer[x][y] = z;
                                colourBuffer[x][y] = poly.getReflective();
                            }
                        } else {
                            depthBuffer[x][y] = z;
                            colourBuffer[x][y] = poly.getReflective();
                        }
                        z += mz;
                        x++;
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
