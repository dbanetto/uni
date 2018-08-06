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
		private String state;
		// Note, you may wish to "factor" the state by having additional variables
		// to represent aspects of the state

		private boolean req1, req2, req3;
		/**
		  * The field containing the Lift.
		  * The signal method will call methods on the intersection to
		  * change the lights
		  */
		private Lift lift;  // the lift that is being controlled.

		// possible actions on the lift that you can perform:
		// lift.moveUp()             to start the lift moving up
		// lift.moveDown()         to start the lift moving down
		// lift.stop()             to stop the lift
		// lift.openDoor()         to start the doors opening
		// lift.closeDoor()       to start the doors closing
		// lift.restartTimer(1000)   to set the time for 1000 milliseconds
		// lift.turnWarningLightOn() to turn the warning light on
		// lift.turnWarningLightOff()to turn the warning light off


	/**
		  * The Constructor is passed the lift that it is controlling.
		  */
		public LiftController(Lift lift) {
				state = "f1_idle";
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
		  *  "doorClosed"   "doorOpened"   "doorMoving"
		  *  "timerExpired"
		  *  "doorSensor"
		  *  "withinCapacity"   "overCapacity"
		  *
		  * You can either have one big method, or you can break it up into
		  * a separate method for each state
		  */
		public void signal(String sensor){

				UI.println("Sensor: "+ sensor + " State : " + state);

				if (state.startsWith("f1"))
				{
					Floor1(sensor);
				}
				
				// General Catch for all requests
				switch (sensor)
				{
					case("request1"):
						req1 = true;
						break;
					case("request2"):
						req2 = true;
						break;
					case("request3"):
						req3 = true;
						break;
				}
				UI.println("State : " + state + " R1:" + req1 + " R2:" + req2 + " R3:" + req3);
			}
	private void Floor1(String sensor)
	{

	}

}
