import java.awt.*;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.Map;
import java.util.List;

public class MapGUI extends GUI {
    private Map<Integer, Intersection> intersections;
    private Map<Integer, Road> roads;
    private double scale;
    private Location screenOrigin;
    private QuadTree<Intersection> intersectionMap;

    public MapGUI() {
        super();
        scale = 1.0;
        screenOrigin = Location.newFromLatLon(Location.CENTRE_LAT, Location.CENTRE_LON);
    }

    @Override
    protected void redraw(Graphics g) {
        if (intersections != null) {
            Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
            Rectangle DrawArea = new Rectangle((offset.x) - 5, (offset.y) - 5,
                                               (int)(this.getDrawingAreaDimension().getWidth() / scale) + 5,
                                               (int)(this.getDrawingAreaDimension().getHeight() / scale) + 5) ;
            System.out.print(DrawArea);
            List<Intersection> toDraw = intersectionMap.get(DrawArea);
            System.out.println(toDraw.size());
            for (Intersection iter : toDraw) {
                iter.draw(g, screenOrigin, scale);
            }
            intersectionMap.draw(g, scale, offset);
        }
        if (roads != null) {
            for (Road rd : roads.values()) {
                rd.draw(g, screenOrigin, scale);
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
                scale *= 1.5; // FIXME: Better scaling
                break;
            case ZOOM_OUT:
                scale /= 1.5; // FIXME: Better scaling
                break;
            case NORTH:
                screenOrigin = screenOrigin.moveBy(0, 5);
                break;
            case SOUTH:
                screenOrigin = screenOrigin.moveBy(0, -5);
                break;
            case EAST:
                screenOrigin = screenOrigin.moveBy(5, 0);
                break;
            case WEST:
                screenOrigin = screenOrigin.moveBy(-5, 0);
                break;
        }
        redraw();
    }

    @Override
    protected void onLoad(File nodes, File road, File segments, File polygons) {
        // TODO: Remove testing code
        intersectionMap = new QuadTree<Intersection>(new Rectangle(-1000,-1000,2000,2000));
        intersections = Intersection.LoadFromFile(nodes, intersectionMap);
        roads = Road.LoadFromFile(road);
        Segment.LoadFromFile(segments, intersections, roads);


        System.out.println(intersectionMap.size());
        System.out.println(intersections.size());
    }

    public static void main(String[] args) {
        new MapGUI();
    }
}
