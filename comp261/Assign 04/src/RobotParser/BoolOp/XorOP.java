package RobotParser.BoolOp;

import Game.Robot;
import RobotParser.*;
import RobotParser.Types.BooleanLiteral;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 09/05/15.
 */
public class XorOP extends ProgramOperator implements Expression {
    private final Expression left, right;

    public XorOP() {
        this.left = null;
        this.right = null;
    }

    private XorOP(Expression left, Expression right) {
        this.left = left;
        this.right = right;
    }

    @Override
    protected Expression parseCondition(Expression left, ProgramStack stack, Scanner scanner) {
        Expression right = ProgramExpression.parse(scanner, stack);
        return new XorOP(left, right);
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        return null;
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        Boolean ab = Util.castBool(left.evaluate(robot, stack));
        Boolean bb = Util.castBool(right.evaluate(robot, stack));

        return new BooleanLiteral(ab ^ bb);
    }

    @Override
    public Type getType() {
        return BooleanLiteral.class;
    }

    @Override
    public String toString() {
        return left + " ^ " + right;
    }
}
