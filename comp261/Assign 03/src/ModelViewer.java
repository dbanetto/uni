import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;

public class ModelViewer extends GUI {
    private Scene scene;
    private Camera camera;
    private int width;
    private int height;
    private float scaleStep;

    public ModelViewer() {
        width = GUI.CANVAS_WIDTH;
        height = GUI.CANVAS_HEIGHT;
        scaleStep = 1.1f;
    }

    @Override
    protected void onLoad(File file) {
        // make sure file type is correct
        if (file.getName().endsWith(".txt")) {
            scene = Scene.loadFromFile(file);
            camera = new Camera(new Vector3D(0f,0f,0f), new Vector3D(0f,0f,0f), new Vector3D(1f,1f,1f));
        } else {
            JOptionPane.showMessageDialog(null, "Error: Invalid file type.\nExpected '*.txt'", "Invalid File type", JOptionPane.ERROR_MESSAGE);
            System.out.println("Invalid file format");
        }
    }

    @Override
    protected void centerScene() {
        if (scene != null) {
            scene.centerCamera(camera, this.width, this.height, true);
            redraw();
        }
    }

    @Override
    protected void onKeyPress(KeyEvent ev) {
        switch (ev.getKeyCode()) {
            // Rotate
            case KeyEvent.VK_S:
                camera.rotateX(0.1f);
                break;
            case KeyEvent.VK_W:
                camera.rotateX(-0.1f);
                break;
            case KeyEvent.VK_D:
                camera.rotateY(0.1f);
                break;
            case KeyEvent.VK_A:
                camera.rotateY(-0.1f);
                break;
            // Scale
            case KeyEvent.VK_Q:
                camera.growScale(scaleStep);
                break;
            case KeyEvent.VK_E:
                camera.growScale(1.0f / scaleStep);
                break;

            // Manual Translate
            case KeyEvent.VK_RIGHT:
                camera.translate(new Vector3D(-10.f, 0, 0));
                break;
            case KeyEvent.VK_LEFT:
                camera.translate(new Vector3D(10.f,0,0));
                break;
            case KeyEvent.VK_UP:
                camera.translate(new Vector3D(0f,10f,0));
                break;
            case KeyEvent.VK_DOWN:
                camera.translate(new Vector3D(0,-10f,0));
                break;

            case KeyEvent.VK_P:
                System.out.println(camera);
                break;

            case KeyEvent.VK_L:
                addLight();
                break;

        }
        this.redraw();
    }

    private void addLight() {
        if (scene != null) {
            // Add a light
            // set rotation to what the camera is current looking through
            // so it should be as if the light source is made where the camera is
            Vector3D rot = new Vector3D(0f,0f,-1f); // vector looking straight down z-axis
            rot = camera.getRotationTransformation().multiply(rot).unitVector();

            LightSource ls = new LightSource(rot);
            ls.setIntensity(new Vector3D(0.5f,0.5f,0.5f)); // dim the light

            scene.lights.add(ls);
        }
    }

    @Override
    protected BufferedImage render() {
        return (scene != null ? scene.render(new Rectangle(0,0,width,height), camera, getAmbientLightColour(), false) : null);
    }

    @Override
    protected void cameraReset() {
        this.camera = new Camera(new Vector3D(0f,0f,0f), new Vector3D(0f,0f,0f), new Vector3D(1f,1f,1f));
        redraw();
    }

    public Color getAmbientLightColour() {
        int[] rgb = this.getAmbientLight();
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    public static void main(String[] args) {
        new ModelViewer();
    }

}
