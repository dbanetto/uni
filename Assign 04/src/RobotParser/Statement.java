package RobotParser;

import Game.Robot;

public interface Statement {
    void execute(Robot robot, ProgramStack stack);
}
