package RobotParser;

import RobotParser.BoolFunc.*;
import RobotParser.IntFunc.Add;
import RobotParser.IntFunc.Divide;
import RobotParser.IntFunc.Multiply;
import RobotParser.IntFunc.Subtract;
import RobotParser.Sensor.*;
import RobotParser.Values.BooleanLiteral;
import RobotParser.Values.IntegerLiteral;
import RobotParser.Values.Variable;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public abstract class ProgramExpression {
    private static final Map<String, Expression> expressions;
    static {
        Map<String, Expression> cond = new HashMap<>();
        cond.put("fuelLeft", new FuelLeft());
        cond.put("oppLR", new OppLR());
        cond.put("oppFB", new OppFB());
        cond.put("numBarrels", new NumBarrels());
        cond.put("barrelLR", new BarrelLR());
        cond.put("barrelFB", new BarrelFB());
        cond.put("wallDist", new WallDist());

        cond.put("add", new Add());
        cond.put("sub", new Subtract());
        cond.put("mul", new Multiply());
        cond.put("div", new Divide());
        cond.put(IntegerLiteral.regex, new IntegerLiteral());

        cond.put("gt", new GreaterThan());
        cond.put("lt", new LessThan());
        cond.put("eq", new EqualTo());
        cond.put("not", new Not());
        cond.put("or", new Or());
        cond.put("and", new And());
        cond.put(BooleanLiteral.regex, new BooleanLiteral());

        cond.put(ProgramVariable.regex, new Variable());

        expressions = cond;
    }

    public static boolean isNext(Scanner scanner) {
        for (String act : expressions.keySet()) {
            if (scanner.hasNext(act)) {
                return true;
            }
        }
        return false;
    }

    public static Expression parse(Scanner scanner, ProgramStack stack) {
        Expression toReturn = null;
        for (String act : expressions.keySet()) {
            if (scanner.hasNext(act)) {
                if (expressions.containsKey(act)) {
                    if (!(expressions.get(act) instanceof Literal)) {
                        Parser.gobble(act, scanner);
                    }
                    toReturn = expressions.get(act).parseExpression(scanner, stack);
                    break;
                } else {
                    Parser.fail("\"" + act + "\" is not a recognised expression", scanner);
                }
            }
        }
        if (toReturn == null) {
            Parser.fail("Failed to recognised expression", scanner);
        }
        return toReturn;
    }

}
