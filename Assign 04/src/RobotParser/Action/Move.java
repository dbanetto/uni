package RobotParser.Action;

import Game.Robot;
import RobotParser.*;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class Move extends ProgramAction {
    private final Expression steps;

    public Move() {
        this.steps =null;
    }

    private Move(Expression steps) {
        this.steps = steps;
    }

    @Override
    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        Expression steps = null;
        if (scanner.hasNext("\\(")) {
            Parser.gobble("\\(", scanner);

            steps = ProgramExpression.parse(scanner, stack);
            Util.CheckTypeError(Integer.class, steps, scanner);

            Parser.require("\\)", "Need closing brackets on actions calls", scanner);
        }
        return new Move(steps);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        if (steps == null) {
            robot.move();
        } else {
            Integer Int = Util.castInt(steps.evaluate(robot, stack));
            for (int i = 0; i < Int.intValue(); i++) {
                robot.move();
            }
        }
    }

    @Override
    public String toString() {
        if (steps == null) {
            return "move;";
        } else {
            return "move(" + steps + ");";
        }
    }
}
