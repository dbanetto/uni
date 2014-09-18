// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

import ecs100.*;

/**
 * This class contains the main method of the program.
 * 
 * A SlideshowApp object represents the slideshow application and sets up the
 * buttons in the UI. It also creates the creator and viewer objects.
 * 
 * These creator and viewer objects implement their own particular behaviour as
 * to how to respond to user input.
 * 
 * @author Thomas Kuehne
 * @version 5 September 2013
 */
public class SlideshowApp implements UIButtonListener {
	private Images images; // A shared reference to a linked list of images.

	private SlideshowCreator creator; // responsible for creating slideshows.
	private SlideshowViewer viewer; // responsible for viewing slideshows.

	private boolean viewerActive; // flag signalling whether the creator or the
									// viewer is active

	// boolean field that can be toggled depending on whether you want to use
	// the
	// recursive or iterative implementations of the linked list methods.
	public static boolean isRecursive = false;

	/**
	 * Constructor for objects of class SlideshowApp One collection of images is
	 * shared between creator and viewer.
	 */
	public SlideshowApp() {
		images = new Images();
		creator = new SlideshowCreator(images);
		viewer = new SlideshowViewer(images);
	}

	/**
	 * Initialises the UI window, and sets up the buttons.
	 */
	public void initialise() {
		UI.initialise();

		UI.addButton("CREATOR mode", this);
		UI.addButton("add before", this);
		UI.addButton("add after", this);
		UI.addButton("remove image", this);
		UI.addButton("remove all", this);
		UI.addButton("go left", this);
		UI.addButton("go right", this);
		UI.addButton("go to start", this);
		UI.addButton("go to end", this);
		UI.addButton("reverse list", this);
		UI.addButton("", null);
		UI.addButton("VIEWER mode", this);
		UI.addButton("start show", this);
		UI.addButton("", null);
		UI.addButton("Toggle Recursive", this);
		UI.addButton("Count", this);
		
		showCreatorUI();

	}

	/**
	 * Activates the creator behaviour
	 * 
	 * Note the switching of the keylistener
	 */
	public void showCreatorUI() {
		viewerActive = false;

		creator.statusScreen();
		UI.setKeyListener(creator);
	}

	/**
	 * Activates the viewer behaviour
	 *
	 * Note the switching of the keylistener
	 */
	public void showViewerUI() {
		viewerActive = true;

		viewer.statusScreen();
		UI.setKeyListener(viewer);
	}

	/**
	 * This method is called when a button that this object is listening to is
	 * pressed.
	 * 
	 * The method first checks whether a slide show is running. If so, all input
	 * is ignored.
	 * 
	 * Then mode changes are checked for.
	 * 
	 * Finally, events are passed on to either the creator or viewer object,
	 * whichever mode the application is in.
	 * 
	 * @param name
	 *            The name of the button pressed.
	 */
	public void buttonPerformed(String name) {
		if (viewer.slideShowIsRunning())
			return;

		if (name.equals("CREATOR mode")) {
			this.showCreatorUI();
		} else if (name.equals("VIEWER mode")) {
			this.showViewerUI();
		} else if (viewerActive) {
			viewer.buttonPerformed(name);
		} else if (name.equals("Toggle Recursive")) {
			SlideshowApp.isRecursive = !SlideshowApp.isRecursive;
			if (SlideshowApp.isRecursive)
				UI.println("Using Recursive methods");
			else
				UI.println("Using iterative methods");
		} else if (name.equals("Count")) {
			UI.println("Item Count : " + this.images.count());
		} else
			creator.buttonPerformed(name);
	}

	public static void main(String[] args) {
		SlideshowApp app = new SlideshowApp();
		app.initialise();
	}

}
