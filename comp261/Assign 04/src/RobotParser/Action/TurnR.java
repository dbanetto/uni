package RobotParser.Action;

import Game.Robot;
import RobotParser.ProgramAction;
import RobotParser.ProgramStack;

import java.util.Scanner;

public class TurnR extends ProgramAction {

    @Override
    public ProgramAction parseAction(Scanner s, ProgramStack stack) {
        return new TurnR();
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        robot.turnRight();
    }

    @Override
    public String toString() {
        return "turnR;";
    }
}
