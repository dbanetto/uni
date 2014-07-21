import ecs100.*;
import java.util.*;

    /**
     * FSM Controller for a simulated Lift.
     * The core of the controller is the signal(String sensor) method
     * which is called by the lift every time a sensor
     * is signalled.
     */
public class LiftController {
	
    /**
     * The field that stores the current state of the FSM
     */
    private String state = "state1";   
    // Note, you may wish to "factor" the state by having additional variables
    // to represent aspects of the state

	
    /**
     * The field containing the Lift.
     * The signal method will call methods on the intersection to
     * change the lights
     */
    private Lift lift;  // the lift that is being controlled.

    // possible actions on the lift that you can perform:
    // lift.moveUp()             to start the lift moving up
    // lift.moveDown()           to start the lift moving down
    // lift.stop()               to stop the lift
    // lift.openDoor()           to start the doors opening
    // lift.closeDoor()          to start the doors closing
    // lift.restartTimer(1000)   to set the time for 1000 milliseconds
    // lift.turnWarningLightOn() to turn the warning light on
    // lift.turnWarningLightOff()to turn the warning light off

    
    /**
     * The Constructor is passed the lift that it is controlling.
     */
    public LiftController(Lift lift) {
	this.lift = lift;
    }
	
    /**
     * Receives a change in a sensor value that may trigger an action and state change.
     * If there is a transition out of the current state associated with this
     * sensor signal, 
     * - it will perform the appropriate action (if any)
     * - it will transition to the next state
     *   (by calling changeToState with the new state).
     *
     * Possible sensor values that you can respond to:
     * (note, you may not need to respond to all of them)
     *   "request1"   "request2"   "request3"
     *   "atF1"   "atF2"   "atF3"
     *   "startUp"   "startDown"
     *    "doorClosed"   "doorOpened"   "doorMoving"
     *    "timerExpired"
     *    "doorSensor"
     *    "withinCapacity"   "overCapacity"
     * 
     * You can either have one big method, or you can break it up into
     * a separate method for each state 
     */
    public void signal(String sensor){
	UI.println("Sensor: "+ sensor);
	if (state.equals("....")){
	    if (sensor.equals("...")){
		// do action
		state = "...";
	    }
	    else if (sensor.equals("...")){
		// do action
		state = "...";
	    }
	}
	else if (state.equals("....")){

	}
	else if (state.equals("....")){

	}
    }
	
	    

    // DRAFT CODE HERE
	    
    //states:
    //    at1_open  x requests  : timer => close,
    //                          : doorsensor => reset,
    //                          : closed ---> at1_closed
    //    at1_closed x requests : request1 => open && reset request 1 && reset timer
    //                          : request2 or request3  => move up ----> movingup
    //    at2_open  x requests  : timer => close,
    //                          :doorsensor => reset,
    //                          : closed ---> at2_closed
    //    at2_closed x requests : request2 => open && reset request2 && reset timer
    //                          : request1  => move down ----> movingdown
    //                          : request3  => move up ----> movingup
    //    at3_open  x requests  : timer => close,
    //                          : doorsensor => reset,
    //                          : closed ---> at3_closed
    //    at3_closed x requests : request1 => open && reset request 1 && reset timer
    //                          : request2 or request3  => move up ----> moving
    //    movingup  x requests  : atf3  => stop ---> atf3_closed
    //                            : atf2 && request2 => stop ---> atf2_closed
    //    movingdown x requests : atf1  => stop ---> atf1_closed
    //                            : atf2 && request2 => stop ---> atf2_closed



    //---- A factored state controller -------------------------
    //    private String state = "closedAt1";
         // or "openAt1" or
         // "doorOpeningAt1" or "doorClosingAt1" or "closedAt1" or
	 // "openAt2" or "doorOpeningAt2" or "doorClosingAt2" or "closedAt2" or
	 // "openAt3" or "doorOpeningAt3" or "doorClosingAt3" or "closedAt3" or
         // "movingup" or "movingdown" 
    private int floorState = 1; // or 2 or 3, or 0 to mean moving between floors
    private String doorState;
    private boolean[] request = new boolean[4];

    public void signalFactored(String sensor){
	UI.println("Sensor: "+ sensor);
	switch (sensor){
	case "doorClosed": doorState = "closed"; break;
	case "doorOpened":
	    doorState = "open";
	    request[floorState] = false;
	    break;
	case "doorMoving": doorState = "moving"; break;
	case "atF1": floorState = 1; break;
	case "atF2": floorState = 2; break;
	case "atF3": floorState = 3; break;
	case "betweenFloors": floorState = 0; break;
	case "request1": request[1] = true; break;
	case "request2": request[2] = true; break;
	case "request3": request[3] = true; break;
	case "timerExpired": lift.closeDoor(); break;
	    
	}
    }


    //END DRAFT CODE	
}
