package RobotParser;
import Game.Robot;

import java.lang.reflect.Type;
import java.util.Scanner;

public interface Expression {
    Expression parseExpression(Scanner scanner, ProgramStack stack);

    ProgramObject evaluate(Robot robot, ProgramStack stack);

    Type getType();

}
