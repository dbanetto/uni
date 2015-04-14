import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;

public class ModelViewer extends GUI {
    private Scene scene;

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
    }

    @Override
    protected BufferedImage render() {
        return null;
    }

    public static void main(String[] args) {
        new ModelViewer();
    }

}
