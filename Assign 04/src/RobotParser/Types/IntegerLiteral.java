package RobotParser.Types;

import Game.Robot;
import RobotParser.*;

import java.lang.reflect.Type;
import java.util.Scanner;


public class IntegerLiteral implements ProgramObject, Literal {
    private final Integer value;
    public static final String regex = "-?[1-9][0-9]*|0";

    public IntegerLiteral() {
        value = null;
    }

    public IntegerLiteral(int val) {
        this.value = val;
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        return this;
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
        return IntegerLiteral.class;
    }

    @Override
    public ProgramObject clone() {
        return new IntegerLiteral(this.value.intValue());
    }

    @Override
    public Object getValue() {
        return value;
    }
}
