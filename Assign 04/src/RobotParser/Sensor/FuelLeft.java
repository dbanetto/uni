package RobotParser.Sensor;

import Game.Robot;
import RobotParser.Expression;
import RobotParser.ProgramStack;
import RobotParser.ProgramObject;
import RobotParser.Types.IntegerLiteral;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class FuelLeft implements Expression {


    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        return new IntegerLiteral(robot.getFuel());
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        return new FuelLeft();
    }

    @Override
    public String toString() {
        return "fuelLeft";
    }

    @Override
    public Type getType() {
        return IntegerLiteral.class;
    }
}
