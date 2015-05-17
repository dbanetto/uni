package RobotParser.Types;

import Game.Robot;
import RobotParser.*;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class BooleanLiteral implements ProgramObject, Literal {
    private final Boolean value;

    public static final BooleanLiteral TRUE = new BooleanLiteral(true);
    public static final BooleanLiteral FALSE = new BooleanLiteral(false);

    public static final String regex = "true|false";
    public BooleanLiteral() {
        value = null;
    }

    public BooleanLiteral(boolean val) {
        this.value = val;
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        if (!scanner.hasNext(regex)) {
            Parser.fail("Expected boolean, \'true\' or \'false\'", scanner);
        }

        return new BooleanLiteral(scanner.nextBoolean());
    }

    public static boolean isNext(Scanner scanner) {
        return scanner.hasNext(regex);
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        return this;
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public Type getType() {
        return BooleanLiteral.class;
    }

    @Override
    public ProgramObject clone() {
        return new BooleanLiteral(this.value.booleanValue());
    }

    @Override
    public Object getValue() {
        return value;
    }
}
