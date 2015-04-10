import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.io.File;
import java.text.DecimalFormat;
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
    private List<RoadSegment> selectedRoadSegments;
    private Set<Intersection> selectedInters;
    private Restriction restrictions;

    private boolean doAStar;
    private Intersection aStarStartPoint;

    private AStarEstimate<Intersection, RoadSegment> distance;
    private AStarEstimate<Intersection, RoadSegment> time;

    private Map<Intersection, TrafficLight> trafficLights;

    public MapGUI() {
        super();
        scale = 1.0;
        screenOrigin = Location.newFromLatLon(Location.CENTRE_LAT, Location.CENTRE_LON);
        selectedRoads = new ArrayList<>(10);
        selectedRoadSegments = new ArrayList<>(10);
        selectedInters = new HashSet<>();
        doAStar = false;

        distance = new AStarEstimate<Intersection, RoadSegment>() {
            @Override
            public double estimate(Intersection from, Intersection goal, RoadSegment extra) {
                return from.location.distance(goal.location);
            }

            @Override
            public double cost(Intersection from, Intersection to, RoadSegment extra) {
                return extra.length;
            }
        };

        time = new AStarEstimate<Intersection, RoadSegment>() {
            @Override
            public double estimate(Intersection from, Intersection goal, RoadSegment extra) {
                double speed = Math.min(extra.parent.getSpeed(), extra.parent.getRoadClassLimit());
                if (goal.getOutOf().size() == 1) {
                    RoadSegment next = goal.getOutOf().iterator().next();
                    speed = ((extra.length * Math.min(extra.parent.getSpeed(), extra.parent.getRoadClassLimit())) +
                            (Math.min(next.parent.getSpeed(), next.parent.getRoadClassLimit()))) / (extra.length + next.length);
                }
                return ((from.location.distance(goal.location)) / speed) * 60;
            }

            @Override
            public double cost(Intersection from, Intersection to, RoadSegment extra) {
                double extraCost = 0;
                if (trafficLights != null) {
                    if (trafficLights.containsKey(to)) {
                        extraCost += 0.5;
                    }
                }

                double speed = Math.min(extra.parent.getSpeed(), extra.parent.getRoadClassLimit());
                return (extra.length / speed) * 60 + extraCost;
            }
        };
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
                if (rd.parent.isOneWay) {
                    g.setColor(Color.cyan);
                } else {
                    g.setColor(Color.black);
                }
                rd.draw(g, screenOrigin, scale);
            }
        }

        if (intersectionIDs != null) {
            // test if the zoom is too small to be rendered
            int scaledSides = Math.min((int)(5 * (scale * 8.0)), 5);
            if (scaledSides >= 0) {
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

        if (selectedRoadSegments.size() > 0) {
            g.setColor(Color.yellow);
            for (RoadSegment seg: selectedRoadSegments) {
                seg.draw(g, screenOrigin, scale);
            }
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
            double clickAccuracy =  Math.max(10 / scale, 5.0); // Click accuracy, bug allows for close clicks at extreme zoom
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
                if (doAStar) {
                    if (aStarStartPoint == null) {
                        aStarStartPoint = found.get(0);
                        aStarStartPoint.setColour(Color.yellow);
                    } else {
                        Intersection endPoint = found.get(0);
                        getTextOutputArea().setText("Starting A*");
                        AStarEstimate<Intersection, RoadSegment> calc = null;
                        String units = "";
                        switch (this.getHeuristicType()) {
                            case DISTANCE:
                                calc = distance;
                                units = "Km";
                                break;
                            case TIME:
                                calc = time;
                                units = "Mins";
                                break;
                            default:
                                throw new RuntimeException("Unsupported Heuristic");
                        }


                        Stack<RoadSegment> segs = new AStar().ShortestPath(aStarStartPoint, endPoint, this.getRoadUsersFlags(), restrictions, calc);
                        getTextOutputArea().setText("Started at " + aStarStartPoint.toString() + " to " + endPoint.toString() + "\n");
                        if (segs != null && segs.size() > 0) {
                            deselectRoadSegments();

                            List<String> streetNames = new ArrayList<>();
                            List<Double> streetLengths = new ArrayList<>();

                            int i = 0;
                            double total = 0.0;
                            while (!segs.isEmpty()) {
                                RoadSegment seg = segs.pop();
                                selectedRoadSegments.add(seg);

                                if (streetNames.size() != 0 && streetNames.get(i).equals(seg.parent.label)) {
                                    streetLengths.set(i, streetLengths.get(i) + calc.cost(seg.from, seg.to, seg));
                                } else {
                                    streetNames.add(seg.parent.label);
                                    streetLengths.add(calc.cost(seg.from, seg.to, seg));
                                    i = streetNames.size() - 1;
                                }
                                total += calc.cost(seg.from, seg.to, seg);
                            }

                            getTextOutputArea().setText("");
                            DecimalFormat dFormat = new DecimalFormat("#.###");
                            for (int n = 0; n < streetNames.size(); n++) {
                                getTextOutputArea().append(streetNames.get(n) + ": " + dFormat.format(streetLengths.get(n)) + units + "\n");
                            }
                            getTextOutputArea().append("Total = " + dFormat.format(total) +  units + "\n");

                        } else {
                            getTextOutputArea().append("Nodes are disconnected");
                        }
                        aStarStartPoint.setColour(Color.blue);
                        aStarStartPoint = null;
                        doAStar = false;
                    }
                } else {
                    selectedInter = found.get(0);
                    getTextOutputArea().setText("ID=" + selectedInter.id + " Paths out=" + selectedInter.getOutOf().size() + "\nIntersects with: " + selectedInter.intersectsWith());
                    selectedInter.setColour(Color.green);
                    deselectRoadSegments();
                    for (RoadSegment s : selectedInter.getOutOf()) {
                        selectedRoadSegments.add(s);
                    }
                    deselectIntersections();
                    for (Intersection n : selectedInter.getNeighbours()) {
                        n.setColour(Color.cyan);
                        selectedInters.add(n);
                    }
                }
            } else {
                getTextOutputArea().setText("");
                if (doAStar) {
                    if (aStarStartPoint != null) {
                        aStarStartPoint.setColour(Color.blue);
                        aStarStartPoint = null;
                    }
                    doAStar = false;
                }
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
            selectedInter.resetColour();
        }
        selectedInter = null;
    }

    private void deselectIntersections() {
        if (selectedInters != null && selectedInters.size() > 0 ) {
            for (Intersection i : selectedInters) {
                i.resetColour();
            }
        }
        selectedInters.clear();
    }

    private void deselectRoadSegments() {
        if (selectedRoadSegments != null && selectedRoadSegments.size() > 0) {
            selectedRoadSegments.clear();
        }
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
    protected void onStartAStar() {
        doAStar = true;
        deselectRoadSegments();
    }

    @Override
    protected void onLoad(File nodes, File road, File segments, File polygons, File restriction, File trafficlights) {
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
        if (restriction != null) {
            restrictions = Restriction.loadRestriction(restriction);
        }
        if (trafficlights != null) {
            trafficLights = TrafficLight.loadTrafficLights(trafficlights, intersectionQuadTree);
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

    @Override
    protected void onIdentifyArticulationPoints() {
        if (intersectionIDs != null && !intersectionIDs.isEmpty()) {
            selectedInters.clear();
            Set<Intersection> artPoints = Articulation.IdentifyArticulationPoints(intersectionIDs.values());
            for (Intersection i : artPoints) {
                selectedInters.add(i);
                i.setColour(Color.pink);
            }
            assert(selectedInters.size() == artPoints.size());
            getTextOutputArea().setText("Art Points: "+ selectedInters.size());
        }
    }

    public static void main(String[] args) {
        new MapGUI();
    }
}
