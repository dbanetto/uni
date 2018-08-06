package RobotParser;

import Game.Robot;

import java.util.Scanner;

public abstract class ProgramStatement implements Statement {

    public static boolean isNext(Scanner scanner) {
        return ProgramAction.isNext(scanner) || ProgramLoop.isNext(scanner)
                || ProgramIf.isNext(scanner) || ProgramWhile.isNext(scanner);
    }

    public static Statement parse(Scanner scanner, ProgramStack stack) {
        if (ProgramAction.isNext(scanner)) {
            return ProgramAction.parse(scanner, stack);
        }
        if (ProgramLoop.isNext(scanner)) {
            return ProgramLoop.parse(scanner, stack);
        }
        if (ProgramIf.isNext(scanner)) {
            return ProgramIf.parse(scanner, stack);
        }
        if (ProgramWhile.isNext(scanner)) {
            return ProgramWhile.parse(scanner, stack);
        }
        if (ProgramVariable.isNext(scanner)) {
            ProgramVariable var = ProgramVariable.parse(scanner, stack);
            Parser.require(";", "variable assignment ends with a \';\'", scanner);
            return var;
        }
        Parser.fail("Failed to recognise statement", scanner);
        return null;
    }

    public abstract void execute(Robot robot, ProgramStack stack);
}
