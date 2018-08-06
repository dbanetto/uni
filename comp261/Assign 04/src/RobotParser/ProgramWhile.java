package RobotParser;

import Game.Robot;

import java.util.Scanner;

public class ProgramWhile implements Statement {
    private final ProgramBlock block;
    private final Expression condition;
    private final boolean doWhile;

    private ProgramWhile(ProgramBlock block, Expression condition, boolean doWhile) {
        this.block = block;
        this.condition = condition;
        this.doWhile = doWhile;
    }

    public static boolean isNext(Scanner scanner) {
        return (scanner.hasNext("while") || scanner.hasNext("do"));
    }

    public static ProgramWhile parse(Scanner scanner, ProgramStack stack) {
        if (scanner.hasNext("while")) {
            Parser.gobble("while", scanner);
            Parser.require("\\(", "missing \'(\'", scanner);

            Expression condition = ProgramExpression.parse(scanner, stack);
            Util.CheckTypeErrorBool(condition, scanner);

            Parser.require("\\)", "missing \')\'", scanner);

            ProgramBlock block = ProgramBlock.parse(scanner, stack);

            return new ProgramWhile(block, condition, false);
        } else if (scanner.hasNext("do")) {
            Parser.gobble("do", scanner);

            ProgramBlock block = ProgramBlock.parse(scanner, stack);

            Parser.require("\\(", "missing \'(\'", scanner);

            Expression condition = ProgramExpression.parse(scanner, stack);
            Util.CheckTypeErrorBool(condition, scanner);

            Parser.require("\\)", "missing \')\'", scanner);

            Parser.require("while", "missing \'while\' at end of do-while loop", scanner);

            return new ProgramWhile(block, condition, true);
        }

        throw new AssertionError();
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        if (doWhile) {
            block.execute(robot, stack);
        }
        Boolean condBool = Util.castBool(condition.evaluate(robot, stack));
        while (condBool.booleanValue()) {
            block.execute(robot, stack);

            condBool = Util.castBool(condition.evaluate(robot, stack));
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (doWhile) {
            sb.append("do ");
            sb.append(block);
            sb.append("while ");
            sb.append(condition);
        } else {
            sb.append("while ");
            sb.append(condition);
            sb.append(block);
        }
        return sb.toString();
    }
}
