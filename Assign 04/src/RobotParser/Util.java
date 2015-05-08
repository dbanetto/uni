package RobotParser;

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
        if (!(obj instanceof Boolean)) {
            throw new RuntimeException("Error: Unexpected type. Expected Boolean got " + obj.getClass());
        }
        return (Boolean)obj;
    }

    public static Integer castInt(Object obj) {
        if (obj == null) {
            throw new NullPointerException();
        }
        if (!(obj instanceof Integer)) {
            throw new RuntimeException("Error: Unexpected type. Expected Integer got " + obj.getClass());
        }
        return (Integer)obj;
    }

    public static void CheckType(Object obj, Type type) {
        if (obj == null) {
            throw new NullPointerException();
        }
        if (obj.getClass() != type) {
            throw new RuntimeException("Error: Unexpected type. Expected " + type + " got " + obj.getClass());
        }
    }

    public static void CheckTypeError(Type expected, Expression e, Scanner s) {
        if (expected != e.getType()) {
            TypeError(expected, e, s);
        }
    }

    public static void TypeError(Type expected, Expression e, Scanner s) {
        Parser.fail("Type error: Expected " + expected + " but got " + e.getType() + " in expression \'" + e + "\'", s);
    }

}
