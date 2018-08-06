package RobotParser;

import Game.Robot;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class ProgramBlock implements Statement {
    protected final List<Statement> statements;

    private ProgramBlock(List<Statement> statements) {
        this.statements = statements;
    }

    protected ProgramBlock() {
        this.statements = new ArrayList<>();
    }

    public static ProgramBlock parse(Scanner scanner, ProgramStack stack) {
        ProgramStack child = stack.makeChild();
        List<Statement> statements = new ArrayList<>();

        Parser.require("\\{", "Blocks must start with a '{'", scanner);
        while (!scanner.hasNext("\\}")) {
            statements.add(ProgramStatement.parse(scanner, child));
        }
        Parser.require("\\}", "Blocks must start with a '}'", scanner);

        if (statements.isEmpty()) {
            Parser.fail("Block is empty", scanner);
        }
        return new ProgramBlock(statements);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        // Go in a scope
        ProgramStack child = stack.makeChild();
        for (Statement ex: statements) {
            ex.execute(robot, child);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        for (Statement exp : statements) {
            sb.append(exp);
        }
        sb.append("}");
        return sb.toString();
    }
}
