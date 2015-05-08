package RobotParser.Values;

import Game.Robot;
import RobotParser.Expression;
import RobotParser.Literal;
import RobotParser.Parser;
import RobotParser.ProgramStack;

import java.lang.reflect.Type;
import java.util.Scanner;


public class IntegerLiteral implements Expression, Literal {
    private final Integer value;
    public static final String regex = "-?[1-9][0-9]*|0";

    public IntegerLiteral() {
        value = null;
    }

    private IntegerLiteral(int val) {
        this.value = val;
    }

    @Override
    public Object evaluate(Robot robot, ProgramStack stack) {
        return value;
    }

    public static boolean isNext(Scanner scanner) {
        return scanner.hasNext(regex);
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        if (!scanner.hasNextInt()) {
            Parser.fail("Expected integer", scanner);
        }

        return new IntegerLiteral(scanner.nextInt());
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public Type getType() {
        return Integer.class;
    }
}
