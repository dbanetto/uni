package RobotParser.Sensor;

import Game.Robot;
import RobotParser.Expression;
import RobotParser.ProgramStack;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class NumBarrels implements Expression {

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        return new NumBarrels();
    }

    @Override
    public Object evaluate(Robot robot, ProgramStack stack) {
        return robot.numBarrels();
    }

    @Override
    public String toString() {
        return "numBarrels";
    }

    @Override
    public Type getType() {
        return Integer.class;
    }
}
