import java.awt.*;
import java.awt.event.MouseEvent;
import java.io.File;

public class MapGUI extends GUI {

    @Override
    protected void redraw(Graphics g) {

    }

    @Override
    protected void onClick(MouseEvent e) {

    }

    @Override
    protected void onSearch() {

    }

    @Override
    protected void onMove(Move m) {

    }

    @Override
    protected void onLoad(File nodes, File roads, File segments, File polygons) {
        // TODO: Remove testing code
        System.out.println(Intersection.LoadFromFile(nodes).size());
        System.out.println(Road.LoadFromFile(roads).size());
        System.out.println(Segment.LoadFromFile(segments).size());
    }

    public static void main(String[] args) {
        new MapGUI();
    }
}
