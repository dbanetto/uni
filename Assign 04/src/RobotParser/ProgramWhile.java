package RobotParser;

import Game.Robot;

import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class ProgramWhile implements Statement {
    private final ProgramBlock block;
    private final Expression condition;

    private ProgramWhile(ProgramBlock block, Expression condition) {
        this.block = block;
        this.condition = condition;
    }

    public static boolean isNext(Scanner scanner) {
        return (scanner.hasNext("while"));
    }

    public static ProgramWhile parse(Scanner scanner, ProgramStack stack) {
        Parser.gobble("while", scanner);
        Parser.require("\\(", "missing \'(\'", scanner);
        Expression condition = ProgramExpression.parse(scanner, stack);
        if (condition.getType() != Boolean.class) {
            Util.TypeError(Boolean.class, condition, scanner);
        }
        Parser.require("\\)", "missing \')\'", scanner);

        ProgramBlock block = ProgramBlock.parse(scanner, stack);

        return new ProgramWhile(block, condition);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        Boolean condBool = Util.castBool(condition.evaluate(robot, stack));
        while (condBool.booleanValue()) {
            block.execute(robot, stack);

            condBool = Util.castBool(condition.evaluate(robot, stack));
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("while ");
        sb.append(condition);
        sb.append(block);
        return sb.toString();
    }
}
