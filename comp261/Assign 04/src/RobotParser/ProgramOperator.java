package RobotParser;

import RobotParser.BoolOp.*;
import RobotParser.IntOP.*;

import RobotParser.Types.BooleanLiteral;
import RobotParser.Types.IntegerLiteral;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

public abstract class ProgramOperator {
    private static final Map<Type, Map<String, ProgramOperator>> operators;
    static {
        Map<Type, Map<String, ProgramOperator>> ops = new HashMap<>();

        Map<String, ProgramOperator> bools = new LinkedHashMap<>(3);
        bools.put("\\|\\|", new OrOP());
        bools.put("\\&\\&", new AndOP());
        bools.put("\\^", new XorOP());

        Map<String, ProgramOperator> ints = new LinkedHashMap<>(4);
        ints.put("\\+", new PlusOP());
        ints.put("\\-", new MinusOP());
        ints.put("\\/", new DivideOP());
        ints.put("\\*", new StarOP());

        ints.put("==", new EqOP());
        ints.put("\\!=", new NotEqOP());
        ints.put("\\<=", new LessEqOP());
        ints.put("\\>=", new GreaterEqOP());
        ints.put("\\<", new LessOP());
        ints.put("\\>", new GreaterOP());

        ops.put(BooleanLiteral.class, bools);
        ops.put(IntegerLiteral.class, ints);
        operators = ops;
    }

    public static boolean isNext(Scanner scanner) {
        for (Type t : operators.keySet()) {
            for (String act : operators.get(t).keySet()) {
                if (scanner.hasNext(act)) {
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean isNext(Scanner scanner, Type type) {
        for (String act : operators.get(type).keySet()) {
            if (scanner.hasNext(act)) {
                return true;
            }
        }
        return false;
    }

    public static Expression parse(Scanner scanner, ProgramStack stack, Expression leftSide) {
        if (!operators.containsKey(leftSide.getType())) {
            throw new RuntimeException();
        }

        for (String op : operators.get(leftSide.getType()).keySet()) {
            if (scanner.hasNext(op)) {
                Parser.gobble(op, scanner);
                return operators.get(leftSide.getType()).get(op).parseCondition(leftSide, stack, scanner);
            }
        }

        Parser.fail("Did not match to any operators", scanner);
        return null;
    }

    protected abstract Expression parseCondition(Expression left, ProgramStack stack, Scanner scanner);
}
