package RobotParser.IntFunc;

import Game.Robot;
import RobotParser.*;
import RobotParser.Types.IntegerLiteral;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 08/05/15.
 */
public class Multiply implements Expression {
    private final Expression a, b;

    public Multiply() {
        super();
        a = null; b = null;
    }

    private Multiply(Expression a, Expression b) {
        this.a = a;
        this.b = b;
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        Parser.require("\\(", "missing \'(\'", scanner);

        Expression a = ProgramExpression.parse(scanner, stack);
        Util.CheckTypeErrorInt(a, scanner);

        Parser.require(",", "missing \',\'", scanner);

        Expression b = ProgramExpression.parse(scanner, stack);
        Util.CheckTypeErrorInt(a, scanner);

        Parser.require("\\)", "missing \')\'", scanner);

        return new Multiply(a, b);
    }
    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        Integer ai = Util.castInt(a.evaluate(robot, stack));
        Integer bi = Util.castInt(b.evaluate(robot, stack));

        return new IntegerLiteral(ai * bi);
    }

    @Override
    public String toString() {
        return "mul(" + a + ", " + b + ')';
    }
    @Override
    public Type getType() {
        return IntegerLiteral.class;
    }
}
