package assignment4.shapes;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Just a simple command line interactive interpreter
 */
public class main {
    public static void main(String[] args) {
        BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
        try {
            System.out.print("> ");
            String line = stdin.readLine();
            Interpreter p = new Interpreter("", new Canvas(256,256, Color.WHITE));
            while (line != null && !line.equals("quit")) {
                try {
                    p.run(line).show();
                } catch (Exception e) {
                    // Colour text, cause why not
                    System.out.println((char)27 + "[31mError: " + e.getLocalizedMessage() + (char)27 + "[00m");
                }
                System.out.print("\n> ");
                line = stdin.readLine();
            }
        } catch (IOException ex) {
            System.out.print(ex.getLocalizedMessage());
        }
    }
}
