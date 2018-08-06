package RobotParser;

import javax.swing.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.regex.Pattern;

/**
 * The parser and interpreter.
 * The top level parse function, a main method for testing, and several
 * utility methods are provided.
 * You need to implement parseProgram and all the rest of the parser.
 */

public class Parser {

    private static Pattern NUMPAT = Pattern.compile("-?\\d+");  //("-?(0|[1-9][0-9]*)");
    private static Pattern OPENPAREN = Pattern.compile("\\(");

    // Useful Patterns
    private static Pattern CLOSEPAREN = Pattern.compile("\\)");
    private static Pattern OPENBRACE = Pattern.compile("\\{");
    private static Pattern CLOSEBRACE = Pattern.compile("\\}");

    /**
     * Top level parse method, called by the Game.World
     */
    public static RobotProgramNode parseFile(File code) {
        Scanner scan = null;
        try {
            scan = new Scanner(code);

            // the only time tokens can be next to each other is
            // when one of them is one of (){},;
            scan.useDelimiter("\\s+|(?=[{}(),;])|(?<=[{}(),;])");

            RobotProgramNode program = parseProgram(scan);  // You need to implement this!!!

            scan.close();
            return program;
        } catch (FileNotFoundException e) {
            System.out.println("Game.Robot program source file not found");
        } catch (ParserFailureException e) {
            System.out.println("Program.Parser error:");
            System.out.println(e.getMessage());
            scan.close();
        }
        return null;
    }

    /**
     * For testing the parser without requiring the world
     */

    public static void main(String[] args) {
        if (args.length > 0) {
            for (String arg : args) {
                File f = new File(arg);
                if (f.exists()) {
                    System.out.println("Parsing '" + f + "'");
                    RobotProgramNode prog = parseFile(f);
                    System.out.println("Parsing completed ");
                    if (prog != null) {
                        System.out.println("================\nProgram:");
                        System.out.println(prog);
                    }
                    System.out.println("=================");
                } else {
                    System.out.println("Can't find file '" + f + "'");
                }
            }
        } else {
            while (true) {
                JFileChooser chooser = new JFileChooser(".");//System.getProperty("user.dir"));
                int res = chooser.showOpenDialog(null);
                if (res != JFileChooser.APPROVE_OPTION) {
                    break;
                }
                RobotProgramNode prog = parseFile(chooser.getSelectedFile());
                System.out.println("Parsing completed");
                if (prog != null) {
                    System.out.println("Program: \n" + prog);
                }
                System.out.println("=================");
            }
        }
        System.out.println("Done");
    }

    /**
     * PROG  ::= STMT+
     */
    static RobotProgramNode parseProgram(Scanner s) {
        RobotProgram program = new RobotProgram();
        program.parse(s);
        return program;     // just so it will compile!!
    }

    //utility methods for the parser
    /**
     * If the next token in the scanner matches the specified pattern,
     * consume the token and return true. Otherwise return false without
     * consuming anything.
     * Useful for dealing with the syntactic elements of the language
     * which do not have semantic content, and are there only to
     * make the language parsable.
     */
    public static boolean gobble(String p, Scanner s) {
        if (s.hasNext(p)) {
            s.next();
            return true;
        } else {
            return false;
        }
    }

    public static boolean gobble(Pattern p, Scanner s) {
        if (s.hasNext(p)) {
            s.next();
            return true;
        } else {
            return false;
        }
    }

    public static String require(String pat, String msg, Scanner s) {
        if (s.hasNext(pat)) {
            return s.next();
        } else {
            fail(msg, s);
            return null;
        }
    }

    /**
     * Report a failure in the parser.
     */
    public static void fail(String message, Scanner s) {
        String msg = message + "\n   @ ...";
        for (int i = 0; i < 5 && s.hasNext(); i++) {
            msg += " " + s.next();
        }
        throw new ParserFailureException(msg + "...");
    }
}

// You could add the node classes here, as long as they are not declared public (or private)
