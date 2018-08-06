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
public class WallDist implements Expression {

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        return new WallDist();
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        return new IntegerLiteral(robot.getDistanceToWall());
    }

    @Override
    public String toString() {
        return "wallDist";
    }

    @Override
    public Type getType() {
        return IntegerLiteral.class;
    }
}
