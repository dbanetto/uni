package whilelang.compiler;

import whilelang.ast.Attribute;
import whilelang.ast.Stmt;
import whilelang.ast.WhileFile;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Stdlib  {

    public static List<WhileFile.MethodDecl> getStdLib() {
        return Arrays.asList(
                readline()
        );
    }

    private static WhileFile.MethodDecl readline() {
        return new WhileFile.MethodDecl(
                "readline",
                new whilelang.ast.Type.Strung(),
                Collections.emptyList(),
                 Arrays.asList(new Stmt.Syscall(Stmt.Syscall.Call.READLINE))
                );
    }

}
