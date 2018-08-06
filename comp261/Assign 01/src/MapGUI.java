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

    private QuadTree<Intersection> intersectionQuadTree;
    private QuadTree<RoadSegment> roadSegmentQuadTree;
    private QuadTree<Polygon> polygonQuadTree;
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
        // protect against in-constructor rendering
        if (screenOrigin == null) { return; }

        Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
        // Enlarge search area by a bit to avoid clipping
        Rectangle DrawArea = new Rectangle((offset.x) - 5, (offset.y) - 5,
                (int)(this.getDrawingAreaDimension().getWidth() / scale) + 5,
                (int)(this.getDrawingAreaDimension().getHeight() / scale) + 5);

        if (polygonQuadTree != null) {
            for (Polygon poly : polygonQuadTree.get(DrawArea)) {
                poly.draw(g, screenOrigin, scale);
            }
        }

        if (roadIDs != null) {
            for (RoadSegment rd : roadSegmentQuadTree.get(DrawArea)) {
                g.setColor(Color.black);
                rd.draw(g, screenOrigin, scale);
            }
        }

        if (intersectionIDs != null) {
            // test if the zoom is too small to be rendered
            int scaledSides = Math.min((int)(5 * (scale * 8.0)), 5);
            if (scaledSides <= 0) {
                for (Intersection iter : intersectionQuadTree.get(DrawArea)) {
                    iter.draw(g, screenOrigin, scale);
                }
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
        if (intersectionQuadTree != null) {
            // Only search if Mouse button 1 is pressed (usually Left button)
            if (e.getButton() != MouseEvent.BUTTON1) { return; }

            Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
            double xprec = e.getX() / this.getDrawingAreaDimension().getWidth();
            double yprec = e.getY() / this.getDrawingAreaDimension().getHeight();

            // Generate area to be searched that is scaled to the screen size and zoom
            double clickAccuracy =  Math.max(10 / scale, 3.0); // Click accuracy, bug allows for close clicks at extreme zoom
            Rectangle search = new Rectangle((int)(xprec * (this.getDrawingAreaDimension().getWidth() / scale) + offset.x),
                                             (int)(yprec * (this.getDrawingAreaDimension().getHeight() / scale) + offset.y),
                    (int)(clickAccuracy),(int)(clickAccuracy));

            deselectIntersection();
            deselectRoads();

            List<Intersection> found = intersectionQuadTree.get(search);
            if (found.size() > 0) {
                if (found.size() > 1) {
                    // Find closest to mouse pointer
                    final int x = (int) (xprec * (this.getDrawingAreaDimension().getWidth() / scale) + offset.x);
                    final int y = (int) (yprec * (this.getDrawingAreaDimension().getHeight() / scale) + offset.y);
                    Collections.sort(found, new Comparator<Intersection>() {
                        @Override
                        public int compare(Intersection o1, Intersection o2) {
                            Point p1 = o1.location.asPoint(Location.CENTRE, 1.0);
                            Point p2 = o1.location.asPoint(Location.CENTRE, 1.0);

                            return ((p1.x - x) * (p1.x - x) + (p1.y - y) * (p1.y - y)) - ((p2.x - x) * (p2.x - x) + (p2.y - y) * (p2.y - y));
                        }
                    });
                }
                selectedInter = found.get(0);
                getTextOutputArea().setText("ID=" + selectedInter.id + "\nIntersects with: "  + selectedInter.intersectsWith());
                selectedInter.setColour(Color.green);
            } else {
                getTextOutputArea().setText("");
            }
        }
    }

    @Override
    protected void onSearch() {
        List<String> completions = roadTrie.autocomplete(this.getSearchBox().getText(), 10);
        if (completions != null) {
            getTextOutputArea().setText(completions.toString());
            deselectRoads();
            deselectIntersection();

            // Add to selected list and colour them
            for (String rLabel: completions) {
                for (Road rd : roadLabel.get(rLabel)) {
                    rd.setColour(Color.green);
                    selectedRoads.add(rd);
                }
            }
        } else {
            getTextOutputArea().setText("Could not find any streets starting with \'" + this.getSearchBox().getText() + "\'");
        }
    }

    /**
     * Un-highlights the selected roads
     */
    private void deselectRoads() {
        if (selectedRoads != null && selectedRoads.size() > 0) {
            for (Road rd : selectedRoads) {
                rd.setColour(Color.black);
            }
            selectedRoads.clear();
        }
    }

    /**
     * Un-highlights the selected intersection
     */
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

        double delta_width;
        double delta_height;
        Location loc;

        switch(m) {
            case ZOOM_IN:
                // limit zooming
                if (scale > 8) {
                    break;
                }
                // Centered zooming
                delta_width = ((getDrawingAreaDimension().getWidth()/scale) - (getDrawingAreaDimension().getWidth()/(scale * scaleDelta))) / 2;
                delta_height = ((getDrawingAreaDimension().getHeight()/scale) - (getDrawingAreaDimension().getHeight()/(scale * scaleDelta))) / 2;
                loc = Location.newFromPoint(new Point((int)delta_width, (int)delta_height), Location.CENTRE, 1.0);

                scale *= scaleDelta;
                screenOrigin = screenOrigin.moveBy(loc.x, loc.y);
                break;
            case ZOOM_OUT:
                // limit zooming
                if (scale < 0.01) {
                    break;
                }
                // Centered zooming
                delta_width = ((getDrawingAreaDimension().getWidth()/scale) - (getDrawingAreaDimension().getWidth()/(scale / scaleDelta))) / 2;
                delta_height = ((getDrawingAreaDimension().getHeight()/scale) - (getDrawingAreaDimension().getHeight()/(scale / scaleDelta))) / 2;
                loc = Location.newFromPoint(new Point((int)delta_width, (int)delta_height), Location.CENTRE, 1.0);

                screenOrigin = screenOrigin.moveBy(loc.x, loc.y);
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
        intersectionQuadTree = new QuadTree<>();
        roadSegmentQuadTree = new QuadTree<>();
        roadTrie = new TrieNode(false);
        roadLabel = new HashMap<>();

        intersectionIDs = Intersection.LoadFromFile(nodes, intersectionQuadTree);
        roadIDs = Road.LoadFromFile(road, roadTrie, roadLabel);
        RoadSegment.LoadFromFile(segments, intersectionIDs, roadIDs, roadSegmentQuadTree);

        if (polygons != null) {
            polygonQuadTree = new QuadTree<>();
            Polygon.loadPolygons(polygons, polygonQuadTree);
        }
    }

    @Override
    protected void onMousePressed(MouseEvent e) {
        if (e.getButton() == MouseEvent.BUTTON1) {
            dragOrigin = e.getPoint();
        }
    }

    @Override
    protected void onMouseDrag(MouseEvent e) {
        if (dragOrigin != null) {
            // Dragging with point clicked is always under the mouse
            int mv_x = (int)(dragOrigin.getX() - e.getX());
            int mv_y = (int)(dragOrigin.getY() - e.getY());
            Location mv_by = Location.newFromPoint(new Point(mv_x, mv_y), Location.CENTRE, scale);
            screenOrigin = screenOrigin.moveBy(mv_by.x, mv_by.y);
            redraw();
        }
        // Update point last dragged from
        dragOrigin = e.getPoint();
    }

    @Override
    protected void onMouseReleased(MouseEvent e) {

        if (e.getButton() == MouseEvent.MOUSE_FIRST) {
            // stop dragging
            dragOrigin = null;
        }
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
