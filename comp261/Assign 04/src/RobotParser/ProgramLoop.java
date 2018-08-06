package RobotParser;

import Game.Robot;

import java.util.Scanner;

public class ProgramLoop implements Statement {
    private final ProgramBlock block;

    private ProgramLoop(ProgramBlock block) {
        this.block = block;
    }

    public static boolean isNext(Scanner scanner) {
        return (scanner.hasNext("loop"));
    }

    public static ProgramLoop parse(Scanner scanner, ProgramStack stack) {
        Parser.gobble("loop", scanner);
        ProgramBlock block = ProgramBlock.parse(scanner, stack);
        return new ProgramLoop(block);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        while (true) {
            block.execute(robot, stack);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("loop ");
        sb.append(block);
        return sb.toString();
    }
}
