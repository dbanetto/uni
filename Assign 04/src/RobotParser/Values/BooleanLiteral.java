package RobotParser.Values;

import Game.Robot;
import RobotParser.Expression;
import RobotParser.Literal;
import RobotParser.Parser;
import RobotParser.ProgramStack;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class BooleanLiteral implements Expression, Literal {
    private final Boolean value;

    public static final String regex = "true|false";
    public BooleanLiteral() {
        value = null;
    }

    private BooleanLiteral(boolean val) {
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
    public Object evaluate(Robot robot, ProgramStack stack) {
        return value;
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public Type getType() {
        return Boolean.class;
    }
}
