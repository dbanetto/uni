package RobotParser;

import Game.Robot;
import RobotParser.Types.BooleanLiteral;

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
        public ProgramObject evaluate(Robot robot, ProgramStack stack) {
            return BooleanLiteral.TRUE;
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
        Parser.require("\\(", "\'(\' needed for if's", scanner);

        Expression cond = ProgramExpression.parse(scanner, stack);
        Util.CheckTypeErrorBool(cond, scanner);

        Parser.require("\\)", "\')\' needed after expression in if's", scanner);

        ProgramBlock block = ProgramBlock.parse(scanner, stack);

        branches.add(new IfTuple(cond, block));

        boolean completed = false;
        while (scanner.hasNext("else|elif")) {
            if (completed) {
                Parser.fail("cannot have any more branches after the \'else\' branch", scanner);
            }
            if (scanner.hasNext("elif")) {
                Parser.gobble("elif", scanner);

                if (branches.isEmpty()) {
                    Parser.fail("Expected only 'if' before", scanner);
                }

                Parser.require("\\(", "\'(\' needed for elif's", scanner);

                cond = ProgramExpression.parse(scanner, stack);
                Util.CheckTypeErrorBool(cond, scanner);

                Parser.require("\\)", "\')\' needed after expression in elif's", scanner);

                block = ProgramBlock.parse(scanner, stack);

                branches.add(new IfTuple(cond, block));
            } else if (scanner.hasNext("else")) {
                Parser.gobble("else", scanner);
                block = ProgramBlock.parse(scanner, stack);

                branches.add(new IfTuple(ELSE, block));
                completed = true;
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
