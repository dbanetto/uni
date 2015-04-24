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
            scene.centerCamera(camera, this.width, this.height);
            redraw();
        }
    }

    @Override
    protected void onKeyPress(KeyEvent ev) {
        switch (ev.getKeyCode()) {
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
            case KeyEvent.VK_Q:
                camera.growScale(scaleStep);
                break;
            case KeyEvent.VK_E:
                camera.growScale(1.0f / scaleStep);
                break;

            case KeyEvent.VK_B:
                camera.rotateZ(0.1f);
                break;
            case KeyEvent.VK_N:
                camera.rotateZ(-0.1f);
                break;

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

        }
        this.redraw();
    }

    @Override
    protected BufferedImage render() {

        return (scene != null ? scene.render(new Rectangle(0,0,width,height), camera, getAmbientLightColour()) : null);
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
