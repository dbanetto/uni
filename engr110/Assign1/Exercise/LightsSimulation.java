import ecs100.*;


public class LightsSimulation implements UIButtonListener{

    private Intersection intersection; 

    private boolean running = true;

    public LightsSimulation(){
	printHelp();
	UI.addButton("Reset", this);
	UI.addButton("Pause/Run", this);
	UI.addButton("Start Generating", this);
	UI.addButton("Stop Generating", this);
	UI.addButton("New Car EW", this);
	UI.addButton("New Car NS", this);
	UI.addButton("E-W car sensor", this);
	UI.addButton("N-S car sensor", this);
	UI.addButton("E-W ped button", this);
	UI.addButton("N-S ped button", this);
	UI.addButton("Quit", this);
	intersection = new Intersection(); 
	intersection.setController(new LightsController(intersection));
	this.run();
    }
	
    /**
     * The main loop, which goes for ever.
     * Every 20 milliseconds, it updates the
     * intersection, as long as the running flag is true.
     * If running is false, it does nothing until running becomes true.
     * This runs in the main thread.
     */
    public void run() {
	while (true){
	    if (running){
		intersection.update();
	    }
	    UI.sleep(20);
	}
    }

    /**
     * Make a new Simulation object (which will create a new intersection
     * and a new controller) and start the run method.
     */
    public static void main(String[] args) {
	new LightsSimulation();
    }

    /**
     * Respond to the buttons.
     */
    public void buttonPerformed(String name) {
	switch (name) {
	case "Reset" : reset(); break;
	case "Pause/Run" : running = !running; break;
	case "Start Generating":
	    intersection.createNewCarEW();
	    intersection.setGenerateCars(true);
	    break;
	case "Stop Generating" : intersection.setGenerateCars(false); break;
	case "New Car EW" : intersection.createNewCarEW(); break;
	case "New Car NS" : intersection.createNewCarNS(); break;
	case "E-W car sensor" : intersection.doEWsensor(); break;
	case "N-S car sensor" : intersection.doNSsensor(); break;
	case "E-W ped button" : intersection.pressEWped(); break;
	case "N-S ped button" : intersection.pressNSped(); break;
	case "Quit" : System.exit(0);
	}
    }

    /**
     * Reset the intersection with new queues and a new controller, and restart it.
     */
    private void reset(){
	running=false;
	UI.sleep(50); // to make sure the current update is complete
	UI.clearGraphics();
	UI.repaintGraphics();
	intersection = new Intersection(); 
	intersection.setController(new LightsController(intersection));
	running = true;
    }

    private void printHelp(){
	UI.println("'Reset': clears the roads and restarts the simulation");
	UI.println("'Pause/Run': pause or start the simulation");
	UI.println("'Start/Stop Generating': turn on or off the automatic");
	UI.println("    car generation");
	UI.println("'New Car EW/NS': generate one new car");
	UI.println("'E-W/N-S car sensor': pretend a car arrived from the");
	UI.println("    East or West / North or South");
	UI.println("'E-W/N-S ped sensor': pretend a pedestrian pressed button");
	UI.println("    on the East or West / North or South");
	UI.println("----------------------------------------------------------\n\n");
    }

}
