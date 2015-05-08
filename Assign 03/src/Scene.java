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
        // Setup default scene and center the whole model at 0,0
        Camera camera = new Camera(new Vector3D(0f,0f,0f), new Vector3D(0f,0f,0f), new Vector3D(1f,1f,1f));
        Vector3D centerOffset = new Vector3D(0,0,0).minus(new Scene(polygons, lights).getSceneCenter());
        Transform center = Transform.newTranslation(centerOffset);
        for (int i = 0; i < polygons.size(); i++) {
            Polygon p = polygons.get(i);
            polygons.set(i, p.applyTransformation(center));
        }

        return new Scene(polygons, lights);
    }

    /**
     * Get the center of the whole scene
     *
     * @return Vector from origin to the center of the scene
     */
    private Vector3D getSceneCenter() {
        float xmax = Float.MIN_VALUE;
        float ymax = Float.MIN_VALUE;
        float zmax = Float.MIN_VALUE;

        float xmin = Float.MAX_VALUE;
        float ymin = Float.MAX_VALUE;
        float zmin = Float.MAX_VALUE;

        for (Polygon p: polygons) {

            Vector3D min = p.getMinVec();
            Vector3D max = p.getMaxVec();

            xmin = Math.min(xmin, min.x);
            ymin = Math.min(ymin, min.y);
            zmin = Math.min(zmin, min.z);

            xmax = Math.max(xmax, max.x);
            ymax = Math.max(ymax, max.y);
            zmax = Math.max(zmax, max.z);

        }

       return new Vector3D((xmax-xmin)/2+xmin,(ymax-ymin)/2+ymin,0);
    }

    /**
     * Moves camera and centers the scene on the output screen and scales to fit
     *
     * @param camera to be moved
     * @param width output screen width
     * @param height output screen height
     * @param rescale should rescale
     */
    public void centerCamera(Camera camera, int width, int height, boolean rescale) {
        float xmax = Float.MIN_VALUE;
        float ymax = Float.MIN_VALUE;
        float zmax = Float.MIN_VALUE;

        float xmin = Float.MAX_VALUE;
        float ymin = Float.MAX_VALUE;
        float zmin = Float.MAX_VALUE;

        Transform transform = camera.getRotationTransformation();
        for (Polygon p: polygons) {

            p = p.applyTransformation(transform);

            Vector3D min = p.getMinVec();
            Vector3D max = p.getMaxVec();

            xmin = Math.min(xmin, min.x);
            ymin = Math.min(ymin, min.y);
            zmin = Math.min(zmin, min.z);

            xmax = Math.max(xmax, max.x);
            ymax = Math.max(ymax, max.y);
            zmax = Math.max(zmax, max.z);

        }


        float scale =  1f;
        if (rescale) {
            scale = Math.min(width / (xmax - xmin), height / (ymax - ymin));
            camera.setScale(new Vector3D(scale, scale, 1f));
        }

        float newX = (((xmax-xmin)/2 * scale));
        float newY = (((ymax-ymin)/2 * scale));

        camera.setPosition(new Vector3D(newX,newY,0));
    }

    public Vector3D getVertexNormal(Vector3D vec) {
        List<Polygon> incident = new ArrayList<>();
        Vector3D out = new Vector3D(0,0,0);
        for (Polygon p : this.polygons) {
            if (p.containsVec(vec)) {
                incident.add(p);
                out.plus(p.getSurfaceNormal());
            }
        }
        return out;
    }

    public BufferedImage render(Rectangle imageBounds, Camera camera, Color ambientLight, boolean useFlat) {
        // init Z-buffer
        float[][] depthBuffer = new float[imageBounds.width][imageBounds.height];
        Color[][] colourBuffer = new Color[imageBounds.width][imageBounds.height];

        // Get matrix
        Transform transform = camera.getTransformation();
        for (Polygon normalPoly: this.polygons) {
            // Apply matrix and get surface normal and bounds for clipping
            Polygon transPoly = normalPoly.applyTransformation(transform);
            Rectangle polyBounds = transPoly.getBondingBox();
            // used for lighting
            Vector3D normalUnitSurface = normalPoly.getSurfaceNormal().unitVector();

            // Rectangular clipping
            if (imageBounds.intersects(polyBounds)) {

                // Check if looking polygon the right way
                Vector3D surfaceTransNormal = transPoly.getSurfaceNormal();
                if (surfaceTransNormal.z < 0) {

                    // Get edge list
                    EdgeListItem[] EL = transPoly.getEdgeList(imageBounds.height);



                    // Loop down Y-axis
                    for (int y = 0; y < EL.length - 1; y++) {
                        if (EL[y] == null) { continue; }

                        // interpolation starting points
                        int x = Math.round(EL[y].getX_left());
                        float z = EL[y].getZ_left();

                        // Z gradient
                        float mz = (EL[y].getZ_right() - EL[y].getZ_left()) / (EL[y].getX_right() - EL[y].getX_left());

                        // float safety check
                        if (Math.abs(EL[y].getX_right() - EL[y].getX_left()) < 0.001) {
                            mz = 0;
                        } else if (Float.isInfinite(mz)) {
                            mz = 0;
                        } else if (Float.isNaN(mz)) {
                            mz = 0;
                        }


                        // Iterate across horz line
                        while (x <= Math.round(EL[y].getX_right())) {
                            // Check if in bounds of the image and the polygon
                            if (x < 0 ||
                                x >= imageBounds.width ||
                                y < 0 && y >= imageBounds.height ||
                                !polyBounds.contains(x, y)) {
                                // move forward in iteration
                                z += mz;
                                x++;
                                continue;
                            }

                            // Apply shading
                            Color pixel;
                            int r = 0, g = 0, b = 0;
                            for (LightSource light : lights) {
                                Color l = Color.black;
                                if (useFlat) {
                                    l = light.computeIllumination(
                                            normalUnitSurface,
                                            ambientLight,
                                            transPoly.getReflective());
                                } else {
                                    Vector3D vertNormals[] = new Vector3D[] {
                                            this.getVertexNormal(normalPoly.getPoints()[0]),
                                            this.getVertexNormal(normalPoly.getPoints()[1]),
                                            this.getVertexNormal(normalPoly.getPoints()[2])
                                    };
                                    Color vertColours[] = new Color[] {
                                            light.computeIllumination(
                                                    vertNormals[0].unitVector(),
                                                    ambientLight,
                                                    transPoly.getReflective()),
                                            light.computeIllumination(
                                                    vertNormals[1].unitVector(),
                                                    ambientLight,
                                                    transPoly.getReflective()),
                                            light.computeIllumination(
                                                    vertNormals[2].unitVector(),
                                                    ambientLight,
                                                    transPoly.getReflective())
                                    };
                                    l = light.computeIlluminationGouraud(vertColours, transPoly.getPoints(),
                                            new Vector3D(x,y,z), ambientLight,
                                            transPoly.getReflective());
                                }
                                // Additive merge of light sources
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
                                depthBuffer[x][y] = z; // special case: 1st pixel written to area
                                colourBuffer[x][y] = pixel;
                            }
                            z += mz;
                            x++;
                        }
                    }
                }
            }
        }

        // Copy to image
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
