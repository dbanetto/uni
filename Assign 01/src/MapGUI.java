import java.awt.*;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.Map;

public class MapGUI extends GUI {
    private Map<Integer, Intersection> intersections;
    private Map<Integer, Road> roads;
    private double scale;
    private Location offset;

    public MapGUI() {
        super();
        scale = 1.0;
        offset = new Location(0.0, 0.0);
    }

    @Override
    protected void redraw(Graphics g) {
        if (intersections != null) {
            for (Intersection iter : intersections.values()) {
                iter.draw(g, scale, offset);
            }
        }
        if (roads != null) {
            for (Road rd : roads.values()) {
                rd.draw(g, scale, offset);
            }
        }
    }

    @Override
    protected void onClick(MouseEvent e) {

    }

    @Override
    protected void onSearch() {

    }

    @Override
    protected void onMove(Move m) {
        switch(m) {
            case ZOOM_IN:
                scale *= 2.0; // FIXME: Better scaling
                break;
            case ZOOM_OUT:
                scale /= 2.0; // FIXME: Better scaling
                break;
            case NORTH:
                offset = offset.moveBy(0.0, 2.5);
                break;
            case SOUTH:
                offset = offset.moveBy(0.0, -2.5);
                break;
            case EAST:
                offset = offset.moveBy(2.5, 0.0);
                break;
            case WEST:
                offset = offset.moveBy(-2.5, 0.0);
                break;
        }
        redraw();
    }

    @Override
    protected void onLoad(File nodes, File road, File segments, File polygons) {
        // TODO: Remove testing code
        intersections = Intersection.LoadFromFile(nodes);
        roads = Road.LoadFromFile(road);
        Segment.LoadFromFile(segments, intersections, roads);
    }

    public static void main(String[] args) {
        new MapGUI();
    }
}
