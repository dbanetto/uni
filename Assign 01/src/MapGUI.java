import java.awt.*;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.*;
import java.util.List;

public class MapGUI extends GUI {
    private Map<Integer, Intersection> intersectionIDs; // <ID, Intersection with ID>
    private Map<Integer, Road> roadIDs; // <ID, Road with ID>
    private Map<String, List<Road>> roadLabel; // <Road Label, Road with Label>
    private double scale;
    private Location screenOrigin;
    private QuadTree<Intersection> intersectionMap;
    private TrieNode roadTrie;

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
        if (roadIDs != null) {
            for (Road rd : roadIDs.values()) {
                rd.draw(g, screenOrigin, scale);
            }
        }
        if (intersectionIDs != null) {
            Point offset = screenOrigin.asPoint(Location.CENTRE, 1.0);
            Rectangle DrawArea = new Rectangle((offset.x) - 5, (offset.y) - 5,
                                               (int)(this.getDrawingAreaDimension().getWidth() / scale) + 5,
                                               (int)(this.getDrawingAreaDimension().getHeight() / scale) + 5) ;
            List<Intersection> toDraw = intersectionMap.get(DrawArea);
            for (Intersection iter : toDraw) {
                iter.draw(g, screenOrigin, scale);
            }
        }
        //ensure selected roads are draw above rest
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

            Rectangle search = new Rectangle((int)(xprec * (this.getDrawingAreaDimension().getWidth() / scale) + offset.x),
                                             (int)(yprec * (this.getDrawingAreaDimension().getHeight() / scale) + offset.y),
                    (int)(5 / scale),(int)(5 / scale)); // Click accuracy

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
        double mv_y = (0.05 * getDrawingAreaDimension().getHeight()) / scale;
        double mv_x = (0.05 * getDrawingAreaDimension().getWidth()) / scale;
        switch(m) {
            case ZOOM_IN:
                scale *= 2; // FIXME: Better scaling
                break;
            case ZOOM_OUT:
                scale /= 2; // FIXME: Better scaling
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
        roadTrie = new TrieNode(false);
        roadLabel = new HashMap<>();
        intersectionIDs = Intersection.LoadFromFile(nodes, intersectionMap);
        roadIDs = Road.LoadFromFile(road, roadTrie, roadLabel);
        Segment.LoadFromFile(segments, intersectionIDs, roadIDs);


        System.out.println(intersectionMap.size());
        System.out.println(intersectionIDs.size());
        System.out.println(roadLabel.size());
        System.out.println(roadIDs.size());
    }

    public static void main(String[] args) {
        new MapGUI();
    }
}
