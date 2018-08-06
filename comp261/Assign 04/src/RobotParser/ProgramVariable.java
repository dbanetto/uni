package RobotParser;

import Game.Robot;
import RobotParser.Types.Variable;

import java.lang.reflect.Type;
import java.util.Scanner;

public class ProgramVariable implements Statement {
    private String name;
    private Type type;
    private Expression expression;

    public static String regex = "\\$[A-Za-z][A-Za-z0-9]*";

    public ProgramVariable() {
    }

    private ProgramVariable(String name, Expression expression) {
        this.name = name;
        this.type = expression.getType();
        this.expression = expression;
    }

    public static boolean isNext(Scanner scanner) {
        return scanner.hasNext(regex);
    }

    public static ProgramVariable parse(Scanner scanner, ProgramStack stack) {
        String name = scanner.next(ProgramVariable.regex);

        Parser.require("=", "Variable needs to be assigned", scanner);
        Expression e = ProgramExpression.parse(scanner, stack);

        Variable var = new Variable(e.getType(), name);
        if (stack.varExists(name)) {
            Variable lastVar = stack.getVar(name);
            if (lastVar.getType() != var.getType()) {
                Parser.fail("Cannot change the type of variable after initialisation. Tried to set " + name
                        + " type: \'" + lastVar.getType()
                        + "\' to " + var.getType(), scanner);
            }
        }

        stack.addVar(name, var);
        return new ProgramVariable(name, e);
    }

    public void execute(Robot robot, ProgramStack stack) {
       ProgramObject value = expression.evaluate(robot, stack);
       if (!stack.varExists(name)) {
           Variable var = new Variable(expression.getType(), name);
           var.setValue(value);
           stack.addVar(name, var);
       } else {
           stack.getVar(name).setValue(value);
       }
    }


    @Override
    public String toString() {
        return name + (expression != null ? " = " + expression + ";" : "");
    }
}
