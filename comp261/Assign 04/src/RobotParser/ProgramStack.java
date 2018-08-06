package RobotParser;

import RobotParser.Types.Variable;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by drb on 08/05/15.
 */
public class ProgramStack {
    private ProgramStack parent = null;
    private Map<String, Variable> vars; // name, value

    public ProgramStack() {
        vars = new HashMap<>();
    }

    public ProgramStack makeChild() {
        ProgramStack child = new ProgramStack();
        child.parent = this;
        return child;
    }

    public boolean varExistsShallow(String name) {
        return vars.containsKey(name);
    }

    public boolean varExists(String name) {
        if (vars.containsKey(name)) {
            return true;
        }
        if (parent != null) {
            return parent.varExists(name);
        }
        return false;
    }

    public Variable getVar(String name) {
        if (vars.containsKey(name)) {
            return vars.get(name);
        }
        if (parent != null) {
            return  parent.getVar(name);
        }
        throw new RuntimeException("Variable "+ name + " does not exist");
    }

    public void addVar(String name, Variable var) {
        this.vars.put(name, var);
    }

}
