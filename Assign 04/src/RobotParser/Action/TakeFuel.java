package RobotParser.Action;

import Game.Robot;
import RobotParser.ProgramAction;
import RobotParser.ProgramStack;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class TakeFuel extends ProgramAction {

    @Override
    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        return new TakeFuel();
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        robot.takeFuel();
    }

    @Override
    public String toString() {
        return "takeFuel;";
    }
}
