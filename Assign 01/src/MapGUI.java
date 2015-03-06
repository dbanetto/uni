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
            System.out.println("render box " + DrawArea);
            List<Intersection> toDraw = intersectionMap.get(DrawArea);
            for (Intersection iter : toDraw) {
                iter.draw(g, screenOrigin, scale);
            }
        }
        if (roads != null) {
            for (Road rd : roads.values()) {
                rd.draw(g, screenOrigin, scale);
            }
        }
    }

    @Override
    protected void onClick(MouseEvent e) {
        if (intersectionMap != null) {
            Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
            double xprec = e.getX() / this.getDrawingAreaDimension().getWidth();
            double yprec = e.getY() / this.getDrawingAreaDimension().getHeight();

            Rectangle search = new Rectangle((int)(xprec * (this.getDrawingAreaDimension().getWidth() / scale) + offset.x),
                                             (int)(yprec * (this.getDrawingAreaDimension().getHeight() / scale) + offset.y),
                                5,5);

            System.out.println("X%,Y% " + xprec + ", " + yprec);
            System.out.println("X,Y " + e.getX() + ", " + e.getY());
            System.out.println("Origin " + screenOrigin);
            System.out.println("Offset " + offset);
            System.out.println("search " + search);
            System.out.println("Graphics " + this.getDrawingAreaDimension());

            List<Intersection> found = intersectionMap.get(search);
            if (found.size() > 0) {
                System.out.println("found " + found.get(0));
                this.getTextOutputArea().setText(found.get(0).toString());
            } else {
                this.getTextOutputArea().setText("");
            }
        }
    }

    @Override
    protected void onSearch() {

    }

    @Override
    protected void onMove(Move m) {
        switch(m) {
            case ZOOM_IN:
                scale *= 2; // FIXME: Better scaling
                break;
            case ZOOM_OUT:
                scale /= 2; // FIXME: Better scaling
                break;
            case NORTH:
                screenOrigin = screenOrigin.moveBy(0, 100);
                break;
            case SOUTH:
                screenOrigin = screenOrigin.moveBy(0, -100);
                break;
            case EAST:
                screenOrigin = screenOrigin.moveBy(100, 0);
                break;
            case WEST:
                screenOrigin = screenOrigin.moveBy(-100, 0);
                break;
        }
        redraw();
    }

    @Override
    protected void onLoad(File nodes, File road, File segments, File polygons) {
        // TODO: Remove testing code
        intersectionMap = new QuadTree<Intersection>();
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
