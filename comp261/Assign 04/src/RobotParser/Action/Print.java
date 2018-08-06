package RobotParser.Action;

import Game.Robot;
import RobotParser.*;

import java.util.Scanner;

/**
 * Created by drb on 08/05/15.
 */
public class Print extends ProgramAction {
    private final Expression expression;

    public Print() {
        this.expression = null;
    }

    private Print(Expression expression) {
        this.expression = expression;
    }

    @Override
    public ProgramAction parseAction(Scanner scanner, ProgramStack stack) {
        Parser.gobble("\\(", scanner);
        Expression expression = ProgramExpression.parse(scanner, stack);
        Parser.require("\\)", "Need closing brackets on actions calls", scanner);

        return new Print(expression);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        System.out.print(expression.evaluate(robot, stack));
    }

    @Override
    public String toString() {
        return "print(" +expression + ");";
    }
}
