package RobotParser;

import RobotParser.Types.BooleanLiteral;
import RobotParser.Types.IntegerLiteral;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 08/05/15.
 */
public class Util {
    public static Boolean castBool(Object obj) {
        if (obj == null) {
            throw new NullPointerException();
        }
        if (!(obj instanceof BooleanLiteral)) {
            throw new RuntimeException("Error: Unexpected type. Expected Boolean got " + obj.getClass());
        }
        BooleanLiteral bl = (BooleanLiteral)obj;
        return (Boolean)(bl.getValue());
    }

    public static Integer castInt(Object obj) {
        if (obj == null) {
            throw new NullPointerException();
        }
        if (!(obj instanceof IntegerLiteral)) {
            throw new RuntimeException("Error: Unexpected type. Expected Integer got " + obj.getClass());
        }
        IntegerLiteral il = (IntegerLiteral) obj;
        return (Integer)(il.getValue());
    }

    public static void CheckType(Object obj, Type type) {
        if (obj == null) {
            throw new NullPointerException();
        }
        if (obj.getClass() != type) {
            throw new RuntimeException("Error: Unexpected type. Expected " + type + " got " + obj.getClass());
        }
    }

    public static void CheckTypeErrorInt(Expression e, Scanner s) {
        CheckTypeError(IntegerLiteral.class, e, s);
    }

    public static void CheckTypeErrorBool(Expression e, Scanner s) {
        CheckTypeError(BooleanLiteral.class, e, s);
    }

    private static void CheckTypeError(Type expected, Expression e, Scanner s) {
        if (expected != e.getType()) {
            TypeError(expected, e, s);
        }
    }

    private static void TypeError(Type expected, Expression e, Scanner s) {
        Parser.fail("Type error: Expected " + expected + " but got " + e.getType() + " in expression \'" + e + "\'", s);
    }

}
