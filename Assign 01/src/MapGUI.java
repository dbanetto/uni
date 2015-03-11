import javax.swing.text.Segment;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.io.File;
import java.util.*;
import java.util.List;

public class MapGUI extends GUI {
    private Map<Integer, Intersection> intersectionIDs; // <Intersection ID, Intersection>
    private Map<Integer, Road> roadIDs; // <RoadID, Road>
    private Map<String, List<Road>> roadLabel; // <Road Label, Roads with Label>

    private QuadTree<Intersection> intersectionMap;
    private QuadTree<RoadSegment> roadSegmentnMap;
    private TrieNode roadTrie;

    private double scale;
    private Location screenOrigin;
    private Point dragOrigin;

    private List<Road> selectedRoads;
    private Intersection selectedInter;

    public MapGUI() {
        super();
        scale = 1.0;
        screenOrigin = Location.newFromLatLon(Location.CENTRE_LAT, Location.CENTRE_LON);
        selectedRoads = new ArrayList<>(10);
    }

    @Override
    protected void redraw(Graphics g) {
        Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
        // Enlarge search area by a bit to avoid clipping
        Rectangle DrawArea = new Rectangle((offset.x) - 5, (offset.y) - 5,
                (int)(this.getDrawingAreaDimension().getWidth() / scale) + 5,
                (int)(this.getDrawingAreaDimension().getHeight() / scale) + 5);

        if (roadIDs != null) {
            for (RoadSegment rd : roadSegmentnMap.get(DrawArea)) {
                g.setColor(Color.black);
                rd.draw(g, screenOrigin, scale);
            }
        }

        if (intersectionIDs != null) {
            for (Intersection iter : intersectionMap.get(DrawArea)) {
                iter.draw(g, screenOrigin, scale);
            }
        }

        // ensure selected roads are draw above rest with proper colour
        if (selectedRoads.size() > 0) {
            Graphics2D g2d = (Graphics2D)g;
            g2d.setStroke(new BasicStroke(2));
            for (Road rd: selectedRoads) {
                rd.draw(g, screenOrigin, scale);
            }
            g2d.setStroke(new BasicStroke(1));
        }
        // ensure the selected Intersection is drawn above the rest
        if (selectedInter != null) {
            selectedInter.draw(g, screenOrigin, scale);
        }
    }

    @Override
    protected void onClick(MouseEvent e) {
        if (intersectionMap != null) {
            Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
            double xprec = e.getX() / this.getDrawingAreaDimension().getWidth();
            double yprec = e.getY() / this.getDrawingAreaDimension().getHeight();

            double clickAccuracy =  5 / scale;
            Rectangle search = new Rectangle((int)(xprec * (this.getDrawingAreaDimension().getWidth() / scale) + offset.x - clickAccuracy/2),
                                             (int)(yprec * (this.getDrawingAreaDimension().getHeight() / scale) + offset.y - clickAccuracy/2),
                    (int)(clickAccuracy),(int)(clickAccuracy)); // Click accuracy

            deselectIntersection();
            deselectRoads();

            List<Intersection> found = intersectionMap.get(search);
            if (found.size() > 0) {
                selectedInter = found.get(0);
                this.getTextOutputArea().setText("ID=" + selectedInter.id + "\nIntersects with: "  + selectedInter.intersectsWith());
                selectedInter.setColour(Color.green);
            } else {
                this.getTextOutputArea().setText("");
            }
        }
    }

    @Override
    protected void onSearch() {
        List<String> completes = roadTrie.autocomplete(this.getSearchBox().getText(), 10);
        if (completes != null) {
            this.getTextOutputArea().setText(completes.toString());
            this.deselectRoads();
            deselectIntersection();
            for (String rLabel: completes) {
                for (Road rd : roadLabel.get(rLabel)) {
                    rd.setColour(Color.green);
                    selectedRoads.add(rd);
                }
            }
        } else {
            this.getTextOutputArea().setText("Could not find any streets starting with \'" + this.getSearchBox().getText() + "\'");
        }
    }

    private void deselectRoads() {
        if (selectedRoads != null && selectedRoads.size() > 0) {
            for (Road rd : selectedRoads) {
                rd.setColour(Color.black);
            }
            selectedRoads.clear();
        }
    }

    private void deselectIntersection() {
        if (selectedInter != null) {
            selectedInter.setColour(Color.blue);
        }
        selectedInter = null;
    }

    @Override
    protected void onMove(Move m) {

        onMove(m, 2.0);
    }

    /**
     *
     * @param m Move
     * @param scaleDelta How much to change the scale by
     */
    protected void onMove(Move m, double scaleDelta) {
        // Disallow changes when map is not loaded
        if (roadIDs == null || intersectionIDs == null) { return; }

        double mv_y = (0.05 * getDrawingAreaDimension().getHeight()) / scale;
        double mv_x = (0.05 * getDrawingAreaDimension().getWidth()) / scale;

        switch(m) {
            case ZOOM_IN:
                scale *= scaleDelta;
                break;
            case ZOOM_OUT:
                scale /= scaleDelta;
                break;
            case NORTH:
                screenOrigin = screenOrigin.moveBy(0, mv_y);
                break;
            case SOUTH:
                screenOrigin = screenOrigin.moveBy(0, -mv_y);
                break;
            case EAST:
                screenOrigin = screenOrigin.moveBy(mv_x, 0);
                break;
            case WEST:
                screenOrigin = screenOrigin.moveBy(-mv_x, 0);
                break;
        }
        redraw();
    }

    @Override
    protected void onLoad(File nodes, File road, File segments, File polygons) {
        // TODO: Remove testing code
        intersectionMap = new QuadTree<Intersection>();
        roadSegmentnMap = new QuadTree<RoadSegment>();
        roadTrie = new TrieNode(false);
        roadLabel = new HashMap<>();
        intersectionIDs = Intersection.LoadFromFile(nodes, intersectionMap);
        roadIDs = Road.LoadFromFile(road, roadTrie, roadLabel);
        RoadSegment.LoadFromFile(segments, intersectionIDs, roadIDs, roadSegmentnMap);


        System.out.println(intersectionMap.size());
        System.out.println(intersectionIDs.size());
        System.out.println(roadLabel.size());
        System.out.println(roadIDs.size());
    }

    @Override
    protected void onMousePressed(MouseEvent e) {
        dragOrigin = e.getPoint();
    }

    @Override
    protected void onMouseDrag(MouseEvent e) {
        if (dragOrigin != null) {
            int mv_x = -(int)(e.getX() - dragOrigin.getX());
            int mv_y = -(int)(e.getY() - dragOrigin.getY());
            Location mv_by = Location.newFromPoint(new Point(mv_x, mv_y), Location.CENTRE, scale);
            screenOrigin = screenOrigin.moveBy(mv_by.x, mv_by.y);
            redraw();
        }
        dragOrigin = e.getPoint();
    }

    @Override
    protected void onMouseReleased(MouseEvent e) {
        dragOrigin = null;
    }

    @Override
    protected void onMouseWheelMoved(MouseEvent e) {
        if (e instanceof MouseWheelEvent) {
            MouseWheelEvent wheel = (MouseWheelEvent)e;
            if (wheel.getWheelRotation() > 0) { // Scroll in
                onMove(Move.ZOOM_OUT, 1.2);
            } else if (wheel.getWheelRotation() < 0) {  // Scroll out
                onMove(Move.ZOOM_IN, 1.2);
            }
        }
    }

    public static void main(String[] args) {
        new MapGUI();
    }
}
