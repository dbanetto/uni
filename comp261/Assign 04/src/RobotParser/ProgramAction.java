package RobotParser;

import RobotParser.Action.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public abstract class ProgramAction implements Statement {
    private static final Map<String, ProgramAction> actions;
    static {
        Map<String, ProgramAction> ac = new HashMap<>();
        ac.put("move", new Move());
        ac.put("turnL", new TurnL());
        ac.put("turnR", new TurnR());
        ac.put("turnAround", new TurnAround());
        ac.put("shieldOn", new ShieldOn());
        ac.put("shieldOff", new ShieldOff());
        ac.put("takeFuel", new TakeFuel());
        ac.put("wait", new Wait());

        // EXT
        ac.put("print", new Print());
        ac.put("println", new Println());

        actions = ac;
    }

    protected ProgramAction() {}

    public static boolean isNext(Scanner scanner) {
        for (String act : actions.keySet()) {
            if (scanner.hasNext(act)) {
                return true;
            }
        }
        return false;
    }

    public abstract ProgramAction parseAction(Scanner scanner, ProgramStack stack);

    public static ProgramAction parse(Scanner scanner, ProgramStack stack) {
        ProgramAction toReturn = null;
        for (String act : actions.keySet()) {
            if (scanner.hasNext(act)) {
                Parser.gobble(act, scanner);
                if (actions.containsKey(act)) {
                    toReturn = actions.get(act).parseAction(scanner, stack);
                    break;
                } else {
                    Parser.fail("\"" + act + "\" is not a recognised action", scanner);
                }
            }
        }
        if (toReturn == null) {
            Parser.fail("Failed to recognised action", scanner);
        }

        Parser.require(";", "Actions end with a ';'", scanner);
        return toReturn;
    }

}
