// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP103, Assignment 3
 * Name: 
 * Usercode: 
 * ID: 
 */

import ecs100.*;

import java.awt.Color;
import java.util.*;
import java.io.*;

/**
 * Program to render a molecule on the graphics pane from different possible
 * perspectives. A molecule consists of a collection of atoms. Each atom has a
 * type (eg, Carbon, or Hydrogen, or Oxygen, ..), and a three dimensional
 * position in the molecule (x, y, z).
 *
 * Each molecule is described in a file by a list of atoms and their positions.
 * The molecule is rendered by drawing a colored circle for each atom. The size
 * and color of each atom is determined by the type of the atom.
 * 
 * To make sure that the nearest atoms appear in front of the furthest atoms,
 * the atoms must be rendered in order from the furthest away to the nearest.
 * Each viewing perspective imposes a different ordering on the atoms.
 *
 * The description of the size and color for rendering the different types of
 * atoms is stored in the file "element-table.txt" which should be read and
 * stored in a Map. When an atom is rendered, the type should be looked up in
 * the map to find the size and color to pass to the atom's render() method
 * 
 * A molecule can be rendered from different perspectives, and the program
 * provides four buttons to control the perspective of the rendering: "Front"
 * renders the molecule from the front (perspective = 0 degrees) "Back" renders
 * the molecule from the back (perspective = 180 degrees) "Left" renders the
 * molecule from the left (perspective = -90 degrees) "Right" renders the
 * molecule from the right (perspective = 90 degrees) "PanLeft" decreases the
 * perspective of the rendering by 5 degrees, "PanRight" increases the
 * perspective of the rendering by 5 degrees,
 */

public class MoleculeRenderer implements UIButtonListener {

	// Fields
	// Map containing the size and color of each atom type.
	private Map<String, Element> elementTable;

	// The collection of the atoms in the current molecule
	private List<Atom> molecule;

	private double currentAngle = 0.0; // current viewing angle (in degrees)

	private double panStep = 5.0;
	private double zoom = 1.0;
	// Constructor:
	/**
	 * Set up the Graphical User Interface and read the file of element data of
	 * each possible type of atom into a Map: where the type is the key and an
	 * ElementInfo object is the value (containing size and color).
	 */
	public MoleculeRenderer() {
		UI.setImmediateRepaint(false);

		UI.addButton("Read", this);
		UI.addButton("From Front", this);
		UI.addButton("From Back", this);
		UI.addButton("From Right", this);
		UI.addButton("From Left", this);
		UI.addButton("Pan Left", this);
		UI.addButton("Pan Right", this);
		UI.setKeyListener(new UIKeyListener() {
			
			@Override
			public void keyPerformed(String key) {
				if (key.contains("a"))
					buttonPerformed("Turn Left");
				if (key.contains("d"))
					buttonPerformed("Turn Right");
			}
		});
		
		UI.addSlider("Zoom Scale %", 0, 200, new UISliderListener() {
			
			@Override
			public void sliderPerformed(String name, double value) {
				zoom = value / 100.0;
				render();
			}
		});
		readElementTable(); // Read the element table first
	}

	/**
	 * Respond to button presses. Most of the presses will set the currentAngle
	 * and sort the list of molecules using the appropriate comparator
	 */
	public void buttonPerformed(String button) {
		if (button.equals("Read")) {
			currentAngle = 0;
			String filename = UIFileChooser.open();
			readMoleculeFile(filename);
			Collections.sort(molecule, new BackToFrontComparator());
		} else if (button.equals("From Front")) { 
			currentAngle = 0;
			Collections.sort(molecule, new BackToFrontComparator());
		} else if (button.equals("From Back")) { 
			currentAngle = 180;
			Collections.sort(molecule, new BackToFrontComparator());
		} else if (button.equals("From Left")) { 
			currentAngle = 90;
			Collections.sort(molecule, new BackToFrontComparator());
		} else if (button.equals("From Right")) { 
			currentAngle = 270;
			Collections.sort(molecule, new BackToFrontComparator());
		}  else if (button.equals("Pan Left")) {
			currentAngle += panStep;
			currentAngle %= 360.0;
			Collections.sort(molecule, new BackToFrontComparator());
		} else if (button.equals("Pan Right")) {
			currentAngle -= panStep;
			currentAngle %= 360.0;
			Collections.sort(molecule, new BackToFrontComparator());
		}

		// render the molecule according to the current ordering
		render();
	}

	/**
	 * Reads the molecule data from a file containing one line for each atom in
	 * the molecule. Each line contains an atom type and the 3D coordinates of
	 * the atom. For each atom, it constructs an Atom object, and adds it to the
	 * List of Atoms in the molecule. To get the color and the size of each
	 * atom, it has to look up the type of the atom in the Map of elements.
	 */
	public void readMoleculeFile(String fname) {
		try {
			/* # YOUR CODE HERE */
			Scanner scan = new Scanner(new File(fname));

			molecule = new ArrayList<Atom>();
			while (scan.hasNext()) {
				String symbl = scan.next();
				int x = scan.nextInt();
				int y = scan.nextInt();
				int z = scan.nextInt();
				Element ele = elementTable.get(symbl);
				molecule.add(new Atom(x, y, z, ele.colour, ele.radius));
			}
			scan.close();
		} catch (IOException ex) {
			UI.println("Reading molecule file " + fname + " failed");
		}
	}

	/**
	 * (Completion) Reads a file containing radius and color information about
	 * each type of atom and stores the info in a Map, using the atom type as a
	 * key
	 */
	private void readElementTable() {
		UI.println("Reading the element table file ...");
		try {
			elementTable = new HashMap<>();
			Scanner scan = new Scanner(new File("element-table.txt"));
			while (scan.hasNext()) {
				String symbl = scan.next();
				int radi = scan.nextInt();
				int r = scan.nextInt();
				int g = scan.nextInt();
				int b = scan.nextInt();

				elementTable.put(symbl, new Element(symbl, radi, new Color(r,
						g, b)));
			}
			scan.close();
		} catch (IOException ex) {
			UI.println("Reading element table file FAILED");
		}
	}

	/**
	 * Render the molecule, according the the current ordering of Atoms in the
	 * List. The Atom's render() method needs the current perspective angle
	 */
	public void render() {
		UI.clearGraphics(false);
		if (molecule != null) {
			Collections.sort(molecule, new BackToFrontComparator());
			for (Atom atom : molecule)
				atom.render(currentAngle, zoom);
		}
		UI.drawString("Vewing Angle : " + currentAngle, 10, 10);
		UI.repaintGraphics();
	}

	// Private comparator classes
	// You will need a comparator class for each different direction
	// used in the buttonPerformed method.
	//
	// Each comparator class should be a Comparator of Atoms, and will define
	// a compare method that compares two atoms.
	// Each comparator should have a compare method.
	// Most of the comparators do not need an explicit constructor and have no
	// fields.
	// However, the comparator for the pan methods may need a field and a
	// constructor

	/** Comparator that will order atoms from back to front */
	private class BackToFrontComparator implements Comparator<Atom> {
		/**
		 * Uses the z coordinates of the two atoms larger z means towards the
		 * back, smaller z means towards the front Returns negative if atom1 is
		 * more to the back than atom2, ( 0 if they are in the same plane,
		 * positive if atom1 is more to the front than atom2.
		 */
		@Override
		public int compare(Atom o1, Atom o2) {
			int f = o1.further(o2, currentAngle);
			if (f > 0)
				return 1;
			else if (f < 0)
				return -1;
			else
				return 0;
		}
	}

	public static void main(String args[]) {
		new MoleculeRenderer();
	}
}
