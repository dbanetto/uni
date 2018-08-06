package RobotParser.Action;

import Game.Robot;
import RobotParser.ProgramAction;
import RobotParser.ProgramStack;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class TurnAround extends ProgramAction {

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        robot.turnAround();
    }

    @Override
    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        return new TurnAround();
    }

    @Override
    public String toString() {
        return "turnAround;";
    }
}
