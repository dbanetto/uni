package RobotParser.Action;

import Game.Robot;
import RobotParser.ProgramAction;
import RobotParser.ProgramStack;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class TurnL extends ProgramAction {

    @Override
    public ProgramAction parseAction(Scanner s, ProgramStack stack) {
        return new TurnL();
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        robot.turnLeft();
    }

    @Override
    public String toString() {
        return "turnL;";
    }
}
