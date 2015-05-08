package RobotParser.BoolFunc;

import Game.Robot;
import RobotParser.*;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class EqualTo implements Expression {
    private final Expression a, b;

    public EqualTo() {
        super();
        a = null; b = null;
    }

    private EqualTo(Expression a, Expression b) {
        this.a = a;
        this.b = b;
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        Parser.require("\\(", "missing \'(\'", scanner);

        Expression a = ProgramExpression.parse(scanner, stack);
        Util.CheckTypeError(Integer.class, a, scanner);

        Parser.require(",", "missing \',\'", scanner);

        Expression b = ProgramExpression.parse(scanner, stack);
        Util.CheckTypeError(Integer.class, b, scanner);

        Parser.require("\\)", "missing \')\'", scanner);

        return new EqualTo(a, b);
    }
    @Override
    public Object evaluate(Robot robot, ProgramStack stack) {
        Integer ai = Util.castInt(a.evaluate(robot, stack));
        Integer bi = Util.castInt(b.evaluate(robot, stack));

        return ai.equals(bi);
    }

    @Override
    public String toString() {
        return "eq(" + a + ", " + b + ')';
    }

    @Override
    public Type getType() {
        return Boolean.class;
    }
}
