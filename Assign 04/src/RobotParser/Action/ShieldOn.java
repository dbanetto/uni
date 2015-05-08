package RobotParser.Action;

import Game.Robot;
import RobotParser.ProgramAction;
import RobotParser.ProgramStack;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class ShieldOn extends ProgramAction {
    @Override
    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        return new ShieldOn();
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        robot.setShield(true);
    }

    @Override
    public String toString() {
        return "shieldOn;";
    }
}

