package RobotParser.Action;

import Game.Robot;
import RobotParser.*;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class Wait extends ProgramAction {
    private final Expression time;

    public Wait() {
        time = null;
    }

    private Wait(Expression time) {
        this.time = time;
    }

    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        Expression time = null;
        if (scanner.hasNext("\\(")) {
            Parser.gobble("\\(", scanner);
            time = ProgramExpression.parse(scanner, stack);
            Util.CheckTypeErrorInt(time, scanner);
            Parser.require("\\)", "Need closing brackets on method calls", scanner);
        }
        return new Wait(time);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        if (time == null) {
            robot.idleWait();
        } else {
            Integer Int = Util.castInt(time.evaluate(robot, stack));
            for (int i = 0; i < Int.intValue(); i++) {
                robot.idleWait();
            }
        }
    }

    @Override
    public String toString() {
       if (time == null) {
           return "wait;";
       } else {
           return "wait(" + time + ");";
       }
    }

}
