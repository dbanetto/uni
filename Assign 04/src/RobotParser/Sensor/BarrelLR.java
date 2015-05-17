package RobotParser.Sensor;

import Game.Robot;
import RobotParser.*;
import RobotParser.Types.IntegerLiteral;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class BarrelLR implements Expression {
    private final Expression expression;

    public BarrelLR() {
        expression = null;
    }
    private BarrelLR(Expression expression) {
        this.expression = expression;
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        if (expression == null) {
            return new IntegerLiteral(robot.getClosestBarrelLR());
        } else {
            Integer Int = Util.castInt(expression.evaluate(robot, stack));
            return new IntegerLiteral(robot.getBarrelFB(Int.intValue()));
        }
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        Expression n = null;
        if (scanner.hasNext("\\(")) {
            Parser.gobble("\\(", scanner);
            n = ProgramExpression.parse(scanner, stack);
            Util.CheckTypeErrorInt(n, scanner);

            Parser.require("\\)", "Sensor with argument must end expression with \')\'", scanner);
        }
        return new BarrelLR(n);
    }

    @Override
    public String toString() {
        return "barrelLR " + (expression != null ? expression : "");
    }

    @Override
    public Type getType() {
        return IntegerLiteral.class;
    }
}
