package RobotParser;

import Game.Robot;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Created by drb on 06/05/15.
 */
public class ProgramIf implements Statement {
    // const conditional for ELSE
    private static final Expression ELSE = new Expression() {
        @Override
        public Expression parseExpression(Scanner scanner, ProgramStack stack) {
            throw new RuntimeException("ELSE cannot parse");
        }

        @Override
        public Object evaluate(Robot robot, ProgramStack stack) {
            return true;
        }

        @Override
        public Type getType() {
            return Boolean.class;
        }
    };

    private final List<IfTuple> branches;

    private ProgramIf(List<IfTuple> branches) {
        this.branches = branches;
    }

    public static boolean isNext(Scanner scanner) {
        return scanner.hasNext("if");
    }

    public static ProgramIf parse(Scanner scanner, ProgramStack stack) {
        List<IfTuple> branches = new ArrayList<>();
        Parser.gobble("if", scanner);
        if (!branches.isEmpty()) {
            Parser.fail("Expected an else before", scanner);
        }
        Parser.gobble("\\(", scanner);
        Expression cond = ProgramExpression.parse(scanner, stack);
        if (cond.getType() != Boolean.class) {
            Util.TypeError(Boolean.class, cond, scanner);
        }
        Parser.gobble("\\)", scanner);

        ProgramBlock block = ProgramBlock.parse(scanner, stack);

        branches.add(new IfTuple(cond, block));

        while (scanner.hasNext("else")) {
            Parser.gobble("else", scanner);
            if (scanner.hasNext("if")) {
                Parser.gobble("if", scanner);

                if (branches.isEmpty()) {
                    Parser.fail("Expected only 'if' before", scanner);
                }

                Parser.gobble("\\(", scanner);
                cond = ProgramExpression.parse(scanner, stack);
                if (cond.getType() != Boolean.class) {
                    Util.TypeError(Boolean.class, cond, scanner);
                }
                Parser.gobble("\\)", scanner);

                block = ProgramBlock.parse(scanner, stack);

                branches.add(new IfTuple(cond, block));
            } else {
                block = ProgramBlock.parse(scanner, stack);

                branches.add(new IfTuple(ELSE, block));
                break;
            }

        }
        if (branches.isEmpty()) {
            Parser.fail("if statements must have more than one branch", scanner);
        }
        return new ProgramIf(branches);
    }

    @Override
    public void execute(Robot robot, ProgramStack stack) {
        for (IfTuple cond: this.branches) {
            Boolean condBool = Util.castBool(cond.conditional.evaluate(robot, stack));
            if (condBool) {
                cond.block.execute(robot, stack);
                break;
            }
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("if ( ");
        sb.append(branches.get(0).conditional);
        sb.append(" )");
        sb.append(branches.get(0).block);
        for (int i = 1; i < branches.size(); i++) {
            if (branches.get(i).conditional != ELSE) {
                sb.append("if ( ");
                sb.append(branches.get(i).conditional);
                sb.append(" )");
            } else {
                sb.append("else ");
            }
            sb.append(branches.get(i).block);
        }
        return sb.toString();
    }

    private static class IfTuple {
        public final Expression conditional;
        public final ProgramBlock block;

        public IfTuple(Expression conditional, ProgramBlock block) {
            this.conditional = conditional;
            this.block = block;
        }
    }
}
