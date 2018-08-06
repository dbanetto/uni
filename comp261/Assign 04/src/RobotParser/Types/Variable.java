package RobotParser.Types;

import Game.Robot;
import RobotParser.*;

import java.lang.reflect.Type;
import java.util.Scanner;

/**
 * Created by drb on 08/05/15.
 */
public class Variable implements Literal, ProgramObject {
    private ProgramObject value;
    private final Type type;
    private final String name;

    public Variable() {
        this.type = null;
        this.name = null;
    }

    public Variable(Type type, String name) {
        this.value = null;
        this.type = type;
        this.name = name;
    }

    @Override
    public Expression parseExpression(Scanner scanner, ProgramStack stack) {
        String name = scanner.next(ProgramVariable.regex);
        if (!stack.varExists(name)) {
            Parser.fail("Usage of " + name + " before it is defined", scanner);
        }
        Variable var = stack.getVar(name);
        return new StackVariableLookUp(name, var.getType());
    }

    @Override
    public ProgramObject evaluate(Robot robot, ProgramStack stack) {
        if (value == null) {
            throw new RuntimeException("Used variable before use");
        }
        return value;
    }

    @Override
    public Type getType() {
        return type;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    public Variable clone() {
        Variable var = new Variable(this.type, this.name);
        var.setValue(this.value);
        return var;
    }

    @Override
    public Object getValue() {
        return value;
    }

    public void setValue(ProgramObject val) {
        if (val != null) {
            Util.CheckType(val, type);
        }
        this.value = val;
    }

    private static class StackVariableLookUp implements Expression {
        private final String name;
        private final Type type;

        StackVariableLookUp(String name, Type type) {
            this.name = name;
            this.type = type;
        }

        @Override
        public Expression parseExpression(Scanner scanner, ProgramStack stack) {
            throw new AssertionError();
        }

        @Override
        public ProgramObject evaluate(Robot robot, ProgramStack stack) {
            Variable var = stack.getVar(name);
            return var.evaluate(robot, stack);
        }

        @Override
        public Type getType() {
            return type;
        }

        @Override
        public String toString() {
            return name;
        }
    }
}
