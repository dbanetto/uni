package RobotParser;

import Game.Robot;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class RobotProgram implements RobotProgramNode {
    private final List<Statement> statements;
    private final ProgramStack stack;

    public RobotProgram() {
        statements = new ArrayList<>();
        stack = new ProgramStack();
    }

    public void parse(Scanner scanner) {
        // Have a parse stack for parse-time type checking & check variable usage
        ProgramStack parseStack = new ProgramStack();
        while (scanner.hasNext()) {
            statements.add(ProgramStatement.parse(scanner, parseStack));
        }
        if (statements.isEmpty()) {
            Parser.fail("No Statements loaded in file", scanner);
        }
    }

    @Override
    public void execute(Robot robot) {
        for (Statement ex: statements) {
            ex.execute(robot, stack);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Statement exp : statements) {
            sb.append(exp);
            sb.append("\n");
        }
        return sb.toString();
    }

}
