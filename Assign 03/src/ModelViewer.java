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
        width = 500;
        height = 500;
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
    protected void onKeyPress(KeyEvent ev) {
        switch (ev.getKeyCode()) {
            case KeyEvent.VK_W:
            case KeyEvent.VK_UP:
                camera.translate(new Vector3D(0, 10, 0));
                break;
            case KeyEvent.VK_S:
            case KeyEvent.VK_DOWN:
                camera.translate(new Vector3D(0, -10 ,0));
                break;
            case KeyEvent.VK_A:
            case KeyEvent.VK_LEFT:
                camera.translate(new Vector3D(10, 0 ,0));
                break;
            case KeyEvent.VK_D:
            case KeyEvent.VK_RIGHT:
                camera.translate(new Vector3D(-10, 0 ,0));
                break;
            case KeyEvent.VK_Q:
                camera.growScale(scaleStep);
                camera.translate(new Vector3D(-width * 0.5f * (1.0f - (1.0f / scaleStep)), 0, 0));
                camera.translate(new Vector3D(0, -height * 0.5f * (1.0f - (1.0f / scaleStep)) ,0));
                break;
            case KeyEvent.VK_E:
                camera.growScale(1.0f / scaleStep);

                camera.translate(new Vector3D(-width * 0.5f * (1.0f - (scaleStep)), 0, 0));
                camera.translate(new Vector3D(0, -height * 0.5f * (1.0f - (scaleStep)), 0));
                break;
            case KeyEvent.VK_Z:
                camera.rotateX(0.1f);
                break;
            case KeyEvent.VK_X:
                camera.rotateX(-0.1f);
                break;
            case KeyEvent.VK_C:
                camera.rotateY(0.1f);
                break;
            case KeyEvent.VK_V:
                camera.rotateY(-0.1f);
                break;
            case KeyEvent.VK_B:
                camera.rotateZ(0.1f);
                break;
            case KeyEvent.VK_N:
                camera.rotateZ(-0.1f);
                break;
        }
        this.redraw();
    }

    @Override
    protected BufferedImage render() {

        return (scene != null ? scene.render(new Rectangle(0,0,width,height), camera, getAmbientLightColour()) : null);
    }

    public Color getAmbientLightColour() {
        int[] rgb = this.getAmbientLight();
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    public static void main(String[] args) {
        new ModelViewer();
    }

}
