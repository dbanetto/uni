package RobotParser.Sensor;

import Game.Robot;
import RobotParser.Expression;
import RobotParser.ProgramStack;

import java.lang.reflect.Type;
import java.util.Scanner;

public class OppLR implements Expression {

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        return new OppLR();
    }

    @Override
    public Object evaluate(Robot robot, ProgramStack stack) {
        return robot.getOpponentLR();
    }

    @Override
    public String toString() {
        return "oppLR";
    }

    @Override
    public Type getType() {
        return Integer.class;
    }
}
