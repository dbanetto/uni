package RobotParser.Action;

import Game.Robot;
import RobotParser.*;

import java.util.Scanner;

/**
 * Created by drb on 08/05/15.
 */
public class Println extends ProgramAction {
    private final Expression expression;

    public Println() {
        this.expression = null;
    }

    private Println(Expression expression) {
        this.expression = expression;
    }

    @Override
    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        Parser.gobble("\\(", scanner);
        Expression expression = ProgramExpression.parse(scanner, stack);
        Parser.require("\\)", "Need closing brackets on actions calls", scanner);

        return new Println(expression);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        System.out.println(expression.evaluate(robot, stack));
    }

    @Override
    public String toString() {
        return "println(" +expression + ");";
    }
}
