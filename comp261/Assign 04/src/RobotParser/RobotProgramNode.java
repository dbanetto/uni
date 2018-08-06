package RobotParser;

import Game.Robot;

import java.util.Scanner;

/**
 * Interface for all nodes that can be executed,
 * including the top level program node
 */

public interface RobotProgramNode {
    void parse(Scanner scanner);

    void execute(Robot robot);
}
