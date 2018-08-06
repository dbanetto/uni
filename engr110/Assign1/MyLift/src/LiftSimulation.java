import ecs100.*;

/**
 * This is the top level class for the lift simulation program.
 * It sets up the interface, and then 
 * it creates a lift object and a set of queues of people (using
 * the provided class files which you cannot edit)
 *
 * It then runs the simulation, but updating the lift and the queues
 * of people waiting, and redrawing the lift and people
 * 
 */

public class LiftSimulation implements UIButtonListener{

    private Lift lift;
    private boolean demo;
    private boolean running = true;

    /**
     * Constructor: Set up interface and lift, and call the run method.
     */
    public LiftSimulation(){
        UI.addButton("Start Generating", this);
        UI.addButton("Stop Generating", this);
        UI.addButton("Pause/Run", this);
        UI.addButton("New Person", this);
        UI.addButton("Press call 3", this);
        UI.addButton("Press call 2", this);
        UI.addButton("Press call 1", this);
        UI.addButton("Press button 3", this);
        UI.addButton("Press button 2", this);
        UI.addButton("Press button 1", this);
        UI.addButton("Built-in controller", this);
        UI.addButton("Your controller", this);
        UI.addButton("Quit", this);

        UI.setImmediateRepaint(false);
        lift = new Lift();
        buttonPerformed("Your controller");
        this.run();
    }
        
    /**
     * Runs the simulation.
     * Infinite loop, but only take steps if the field running is true.
     */
    public void run() {
    	while (true){
            if (running){
                this.lift.update();
                this.lift.draw();
            }
            UI.sleep(20);
        }
    }

    /**
     * Respond to the buttons
     */
    public void buttonPerformed(String name) {
        switch (name) {
        case "Built-in controller" : 
        	lift.setController(null); 
        	lift.reset(); 
        	break;
        case "Your controller" :
            lift.setController(new LiftController(lift));
            lift.reset();
            break;
        case "Pause/Run" : running = !running; break;
        case "Start Generating":
            lift.generateRandomPerson();
            lift.setGeneratePeople(true);
            break;
        case "Stop Generating" : lift.setGeneratePeople(false); break;
        case "New Person"      : lift.generateRandomPerson(); break;
        case "Press call 3"    : lift.setCall(3,true); break;
        case "Press call 2"    : lift.setCall(2,true); break;
        case "Press call 1"    : lift.setCall(1,true); break;
        case "Press button 3" : lift.setButton(3,true); break;
        case "Press button 2" : lift.setButton(2,true); break;
        case "Press button 1" : lift.setButton(1, true); break;
        case "Quit" : System.exit(0);
        }
    }

    public static void main(String[] args) {
        new LiftSimulation();
    }

}
