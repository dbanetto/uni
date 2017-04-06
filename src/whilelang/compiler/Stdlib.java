package whilelang.compiler;

import whilelang.ast.Attribute;
import whilelang.ast.Stmt;
import whilelang.ast.WhileFile;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Stdlib  {

    /**
     * Returns a list of methods that are defined as the standard library for While
     *
     * These methods are either general utility methods or methods that
     * cannot be implemented with only Whiley (such as I/O)
     **/
    public static List<WhileFile.MethodDecl> getStdLib() {
        return Arrays.asList(
                readline()
        );
    }

    /**
     * method to read in a line of text from STDIN
     *
     * while signature: string readline()
     *
     * @return method for <code>readline</code>
     */
    private static WhileFile.MethodDecl readline() {
        return new WhileFile.MethodDecl(
                "readline",
                new whilelang.ast.Type.Strung(),
                Collections.emptyList(),
                 Arrays.asList(new Stmt.Syscall(Stmt.Syscall.Call.READLINE))
                );
    }

}
