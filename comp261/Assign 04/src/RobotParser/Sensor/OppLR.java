package RobotParser.Sensor;

import Game.Robot;
import RobotParser.Expression;
import RobotParser.ProgramStack;
import RobotParser.ProgramObject;
import RobotParser.Types.IntegerLiteral;

import java.lang.reflect.Type;
import java.util.Scanner;

public class OppLR implements Expression {

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        return new OppLR();
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        return new IntegerLiteral(robot.getOpponentLR());
    }

    @Override
    public String toString() {
        return "oppLR";
    }

    @Override
    public Type getType() {
        return IntegerLiteral.class;
    }
}
