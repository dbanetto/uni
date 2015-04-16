import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;

public class ModelViewer extends GUI {
    private Scene scene;
    private Vector3D cameraPosition;

    public ModelViewer() {
        this.cameraPosition = new Vector3D(0f,0f,0f);
    }

    @Override
    protected void onLoad(File file) {
        scene = Scene.loadFromFile(file);
    }

    @Override
    protected void onKeyPress(KeyEvent ev) {
        switch (ev.getKeyCode()) {
            case KeyEvent.VK_W:
            case KeyEvent.VK_UP:
                break;
            case KeyEvent.VK_S:
            case KeyEvent.VK_DOWN:
                break;
            case KeyEvent.VK_A:
            case KeyEvent.VK_LEFT:
                break;
            case KeyEvent.VK_D:
            case KeyEvent.VK_RIGHT:
                break;
        }
        this.redraw();
    }

    @Override
    protected BufferedImage render() {

        return (scene != null ? scene.render(new Rectangle(0,0,500,500), cameraPosition) : null);
    }

    public Color getAmbientLightColour() {
        int[] rgb = this.getAmbientLight();
        return new Color(rgb[0], rgb[1], rgb[2]);
    }

    public static void main(String[] args) {
        new ModelViewer();
    }

}
