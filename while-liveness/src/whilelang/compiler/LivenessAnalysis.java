package whilelang.compiler;

import sun.plugin.dom.exception.InvalidStateException;
import whilelang.ast.Attribute;
import whilelang.ast.Expr;
import whilelang.ast.Stmt;
import whilelang.ast.WhileFile;
import whilelang.util.Pair;
import whilelang.util.SyntacticElement;
import whilelang.util.SyntaxError;

import java.util.*;

import static whilelang.util.SyntaxError.internalFailure;
import static whilelang.util.SyntaxError.syntaxError;

public class LivenessAnalysis {

    private String fileName;

    public void check(WhileFile wf) {

        fileName = wf.filename;

        for (WhileFile.Decl declaration : wf.declarations) {
            if (declaration instanceof WhileFile.MethodDecl) {
                check((WhileFile.MethodDecl) declaration);
            }
        }
    }

    public void check(WhileFile.MethodDecl fd) {
        Context environment = new Context();

        for (WhileFile.Parameter p : fd.getParameters()) {
            environment.defined(p.name(), p);
        }

        check(fd.getBody(), environment);

        environment.defined.entrySet().stream()
                .filter(s -> !environment.used.contains(s.getKey()))
                .forEach(s -> printUnusedWarning(s.getKey(), s.getValue()));
    }

    private void printUnusedWarning(String variable, SyntacticElement element) {
        int start = -1;
        int end = -1;

        Attribute.Source attr = element.attribute(Attribute.Source.class);
        if(attr != null) {
            start=attr.start;
            end=attr.end;
        }

        String typeOfVariable = "local variable";
        if  (element instanceof WhileFile.Parameter) {
            typeOfVariable = "parameter";
        }

        SyntaxError warning = new SyntaxError(String.format("WARNING Unused %s `%s`", typeOfVariable, variable), fileName, start, end);
        warning.outputSourceError(System.err);
    }

    /**
     * Check that all variables used in a given list of statements are
     * definitely assigned. Furthermore, update the set of definitely assigned
     * variables to include any which are definitely assigned at the end of
     * these statements.
     *
     * @param statements
     *            The list of statements to check.
     * @param environment
     *            The set of variables which are definitely assigned.
     */
    public void check(List<Stmt> statements, Context environment) {
        for (Stmt s : statements) {
            check(s, environment);
        }
    }

    /**
     * Check that all variables used in a given statement are definitely
     * assigned. Furthermore, update the set of definitely assigned variables to
     * include any which are definitely assigned after this statement.
     *
     * @param stmt
     *            The statement to check.
     * @param environment
     *            The set of variables which are definitely assigned.
     * @return The updated set of variables which are now definitely assigned,
     *         or null if the method has terminated.
     */
    public void check(Stmt stmt, Context environment) {
        if (stmt instanceof Stmt.Assert) {
            check((Stmt.Assert) stmt, environment);
        } else if (stmt instanceof Stmt.Assign) {
            check((Stmt.Assign) stmt, environment);
        } else if (stmt instanceof Stmt.Break) {
            check((Stmt.Break) stmt, environment);
        } else if (stmt instanceof Stmt.Continue) {
            check((Stmt.Continue) stmt, environment);
        } else if (stmt instanceof Stmt.Print) {
            check((Stmt.Print) stmt, environment);
        } else if (stmt instanceof Stmt.Return) {
            check((Stmt.Return) stmt, environment);
        } else if (stmt instanceof Stmt.VariableDeclaration) {
            check((Stmt.VariableDeclaration) stmt, environment);
        } else if (stmt instanceof Expr.Invoke) {
            check((Expr.Invoke) stmt, environment);
        } else if (stmt instanceof Stmt.IfElse) {
            check((Stmt.IfElse) stmt, environment);
        } else if (stmt instanceof Stmt.For) {
            check((Stmt.For) stmt, environment);
        } else if (stmt instanceof Stmt.While) {
            check((Stmt.While) stmt, environment);
        } else if (stmt instanceof Stmt.Switch) {
            check((Stmt.Switch) stmt, environment);
        } else {
            internalFailure("unknown statement encountered (" + stmt + ")", fileName, stmt);
        }
    }

    public void check(Stmt.Assert stmt, Context environment) {
        check(stmt.getExpr(), environment);
    }

    public void check(Stmt.Assign stmt, Context environment) {
        check(stmt.getRhs(), environment);
        if (stmt.getLhs() instanceof Expr.Variable) {
            Expr.Variable var = (Expr.Variable) stmt.getLhs();

            environment.defined(var.getName(), stmt);
        } else {
            check(stmt.getLhs(), environment);
        }
    }

    public void check(Stmt.Break stmt, Context environment) {
        // Here we just move the current environment into the "break"
        // control-flow position.
    }

    public void check(Stmt.Continue stmt, Context environment) {
        // Here we can just treat a continue in the same way as a return
        // statement. It makes no real difference.
    }

    public void check(Stmt.Print stmt, Context environment) {
        check(stmt.getExpr(), environment);
    }

    public void check(Stmt.Return stmt, Context environment) {
        if(stmt.getExpr() != null) {
            check(stmt.getExpr(), environment);
        }
        // In this case, control does not continue after this statement so we
        // return no execution path.
    }

    public void check(Stmt.VariableDeclaration stmt, Context environment) {
        if (environment.contains(stmt.getName())) {
            syntaxError("variable already declared: " + stmt.getName(), fileName, stmt);
        } else if (stmt.getExpr() != null) {
            check(stmt.getExpr(), environment);
            environment.defined(stmt.getName(), stmt);
        }
    }

    public void check(Stmt.IfElse stmt, Context environment) {
        check(stmt.getCondition(), environment);
        check(stmt.getTrueBranch(), environment);
        check(stmt.getFalseBranch(), environment);
    }

    public void check(Stmt.For stmt, Context environment) {
        check(stmt.getDeclaration(), environment);
        check(stmt.getCondition(), environment);
        check(stmt.getIncrement(), environment);

        check(stmt.getBody(), environment);
    }

    public void check(Stmt.While stmt, Context environment) {
        check(stmt.getCondition(), environment);
        check(stmt.getBody(), environment);
    }

    public void check(Stmt.Switch stmt, Context environment) {
        check(stmt.getExpr(), environment);
        for(Stmt.Case c : stmt.getCases()) {
            check(c.getBody(), environment);
        }
    }

    /**
     * Check that all variables used in a given expression are definitely
     * assigned.
     *
     * @param expr
     *            The expression to check.
     * @param environment
     *            The set of variables which are definitely assigned.
     */
    public void check(Expr expr, Context environment) {
        if (expr instanceof Expr.Binary) {
            check((Expr.Binary) expr, environment);
        } else if (expr instanceof Expr.Constant) {
            check((Expr.Constant) expr, environment);
        } else if (expr instanceof Expr.IndexOf) {
            check((Expr.IndexOf) expr, environment);
        } else if (expr instanceof Expr.Invoke) {
            check((Expr.Invoke) expr, environment);
        } else if (expr instanceof Expr.ArrayGenerator) {
            check((Expr.ArrayGenerator) expr, environment);
        } else if (expr instanceof Expr.ArrayInitialiser) {
            check((Expr.ArrayInitialiser) expr, environment);
        } else if (expr instanceof Expr.RecordAccess) {
            check((Expr.RecordAccess) expr, environment);
        } else if (expr instanceof Expr.RecordConstructor) {
            check((Expr.RecordConstructor) expr, environment);
        } else if (expr instanceof Expr.Unary) {
            check((Expr.Unary) expr, environment);
        } else if (expr instanceof Expr.Variable) {
            check((Expr.Variable) expr, environment);
        } else {
            internalFailure("unknown expression encountered (" + expr + ")", fileName, expr);
        }
    }

    public void check(Expr.Binary expr, Context environment) {
        check(expr.getLhs(), environment);
        check(expr.getRhs(), environment);
    }

    public void check(Expr.Constant expr, Context environment) {
        // Constants are obviously already defined ;)
    }

    public void check(Expr.IndexOf expr, Context environment) {
        check(expr.getSource(), environment);
        check(expr.getIndex(), environment);
    }

    public void check(Expr.Invoke expr, Context environment) {
        for (Expr arg : expr.getArguments()) {
            check(arg, environment);
        }
    }

    public void check(Expr.ArrayGenerator expr, Context environment) {
        check(expr.getValue(), environment);
        check(expr.getSize(), environment);
    }

    public void check(Expr.ArrayInitialiser expr, Context environment) {
        for (Expr arg : expr.getArguments()) {
            check(arg, environment);
        }
    }

    public void check(Expr.RecordAccess expr, Context environment) {
        check(expr.getSource(), environment);
    }

    public void check(Expr.RecordConstructor expr, Context environment) {
        for (Pair<String, Expr> arg : expr.getFields()) {
            check(arg.second(), environment);
        }
    }

    public void check(Expr.Unary expr, Context environment) {
        check(expr.getExpr(), environment);
    }

    public void check(Expr.Variable expr, Context environment) {
        environment.used(expr.getName());
    }

    /**
     * A simple class representing an immutable set of definitely assigned
     * variables.
     *
     * @author David J. Pearce
     *
     */
    private class Context {
        private Map<String, SyntacticElement> defined;
        private Set<String> used;

        public Context() {
            this.defined = new HashMap<>();
            this.used = new HashSet<>();
        }

        public Context(Context defs) {
            this.defined = new HashMap<>(defs.defined);
            this.used = new HashSet<>(defs.used);
        }

        public boolean contains(String var) {
            return defined.keySet().contains(var);
        }

        /**
         * Add a variable to the set of definitely assigned variables, producing
         * an updated set.
         *
         * @param var
         * @return
         */
        public void defined(String var, SyntacticElement elem) {
            this.defined.put(var, elem);
        }

        public void used(String var) {
            if (!this.contains(var)) {
                throw new InvalidStateException("Can only be used if defined");
            }
            this.used.add(var);
        }

        @Override
        public String toString() {
            return "Context{" +
                    "defined=" + defined +
                    ", used=" + used +
                    '}';
        }
    }

}
