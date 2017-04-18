package whilelang.compiler;

import jasm.attributes.Code;
import jasm.io.ClassFileWriter;
import jasm.io.JasmFileWriter;
import jasm.lang.*;
import whilelang.ast.*;
import whilelang.util.SyntacticElement;
import whilelang.util.SyntaxError;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author David Barnett
 */
public class JvmCompiler {

    private final String fileName;
    private final String className;

    public JvmCompiler(String fileName, String className) {
        this.fileName = fileName;
        this.className = className;
    }

    public void write(WhileFile ast) {
        try {
            List<Modifier> classModifiers = new ArrayList<>();
            classModifiers.add(Modifier.ACC_PUBLIC);

            ClassFileWriter classWriter = new ClassFileWriter(new FileOutputStream(fileName));

            ClassFile classFile = new ClassFile(49, // java 1.5 or later
                    new JvmType.Clazz("", className),
                    JvmTypes.JAVA_LANG_OBJECT,
                    Collections.EMPTY_LIST,
                    classModifiers
                    );

            for (WhileFile.Decl decl : ast.declarations) {

                if (decl instanceof WhileFile.MethodDecl) {
                    compileMethod(classFile, (WhileFile.MethodDecl) decl);
                }
            }

            classWriter.write(classFile);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Generate bytecode and method call for class
     *
     * @param classFile
     * @param methodDecl
     */
    private void compileMethod(ClassFile classFile, WhileFile.MethodDecl methodDecl) {

        /// ------ setup method ------
        List<Modifier> funcModifiers = new ArrayList<>();
        funcModifiers.add(Modifier.ACC_STATIC);

        // special case where main is a public method
        if (methodDecl.getName().equals("main")) {
            funcModifiers.add(Modifier.ACC_PUBLIC);
        }

        List<JvmType> parameterTypes = new ArrayList<>(methodDecl.getParameters().size());
        for (WhileFile.Parameter param : methodDecl.getParameters()) {
            parameterTypes.add(convertType(param.getType()));
        }

        ClassFile.Method method = new ClassFile.Method(
                methodDecl.name(),
                new JvmType.Function(
                        convertType(methodDecl.getRet()),
                        parameterTypes),
                funcModifiers);

        classFile.methods().add(method);

        // --------- build bytecode of method ---------
        List<Bytecode> bytecode = new ArrayList<>();

        // build
        compile(methodDecl.getBody(), bytecode);

        method.attributes().add(new Code(bytecode, Collections.EMPTY_LIST, method));
    }

    private void compile(List<Stmt> statements, List<Bytecode> bytecode) {
        for (Stmt stmt : statements) {
            compile(stmt, bytecode);
        }
    }
    private void compile(Stmt stmt, List<Bytecode> bytecode) {
        if (stmt instanceof Stmt.Assert) {
            compile((Stmt.Assert) stmt, bytecode);
        } else if (stmt instanceof Stmt.Assign) {
            compile((Stmt.Assign) stmt, bytecode);
        } else if (stmt instanceof Stmt.Print) {
            compile((Stmt.Print) stmt, bytecode);
        } else if (stmt instanceof Stmt.Return) {
            compile((Stmt.Return) stmt, bytecode);
        } else if (stmt instanceof Stmt.Break) {
            compile((Stmt.Break) stmt, bytecode);
        } else if (stmt instanceof Stmt.Continue) {
            compile((Stmt.Continue) stmt, bytecode);
        } else if (stmt instanceof Stmt.VariableDeclaration) {
            compile((Stmt.VariableDeclaration) stmt, bytecode);
        } else if (stmt instanceof Expr.Invoke) {
            compile((Expr.Invoke) stmt, false, bytecode);
        } else if (stmt instanceof Stmt.IfElse) {
            compile((Stmt.IfElse) stmt, bytecode);
        } else if (stmt instanceof Stmt.For) {
            compile((Stmt.For) stmt, bytecode);
        } else if (stmt instanceof Stmt.While) {
            compile((Stmt.While) stmt, bytecode);
        } else if (stmt instanceof Stmt.Switch) {
            compile((Stmt.Switch) stmt, bytecode);
        } else {
            internalFailure("unknown statement encountered (" + stmt + ")", stmt);
        }
    }

    // private void compile(Stmt.Impl stmt, List<Bytecode> bytecode)

    private void compile(Stmt.VariableDeclaration stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Assert stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Break stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Assign stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Continue stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.For stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.IfElse stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Print stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Return stmt, List<Bytecode> bytecode) {
        if (stmt.getExpr() != null) {
            compile(stmt.getExpr(), bytecode);
        }

        bytecode.add(new Bytecode.Return(null)); // TODO: get return type from method
    }
    private void compile(Stmt.Switch stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.Case stmt, List<Bytecode> bytecode) {

    }
    private void compile(Stmt.While stmt, List<Bytecode> bytecode) {

    }

    private void compile(Expr expr, List<Bytecode> bytecode) {

        if (expr instanceof Expr.Binary) {
            compile((Expr.Binary) expr, bytecode);
        } else if (expr instanceof Expr.Constant) {
            compile((Expr.Constant) expr, bytecode);
        } else if (expr instanceof Expr.IndexOf) {
            compile((Expr.IndexOf) expr, bytecode);
        } else if (expr instanceof Expr.Invoke) {
            compile((Expr.Invoke) expr, true, bytecode);
        } else if (expr instanceof Expr.ArrayGenerator) {
            compile((Expr.ArrayGenerator) expr, bytecode);
        } else if (expr instanceof Expr.ArrayInitialiser) {
            compile((Expr.ArrayInitialiser) expr, bytecode);
        } else if (expr instanceof Expr.RecordAccess) {
            compile((Expr.RecordAccess) expr, bytecode);
        } else if (expr instanceof Expr.RecordConstructor) {
            compile((Expr.RecordConstructor) expr, bytecode);
        } else if (expr instanceof Expr.Unary) {
            compile((Expr.Unary) expr, bytecode);
        } else if (expr instanceof Expr.Variable) {
            compile((Expr.Variable) expr, bytecode);
        } else {
            internalFailure("unknown expression encountered (" + expr + ")", expr);
        }
    }


    private void compile(Expr.Invoke expr, boolean inExpression, List<Bytecode> bytecode) {
        throw new UnsupportedOperationException();
    }

    private void compile(Expr.ArrayGenerator expr, List<Bytecode> bytecode) {
        throw new UnsupportedOperationException();
    }
    private void compile(Expr.ArrayInitialiser expr, List<Bytecode> bytecode) {
        throw new UnsupportedOperationException();
    }
    private void compile(Expr.Binary expr, List<Bytecode> bytecode) {
        expr.attributes();
        compile(expr.getLhs(), bytecode);
        compile(expr.getRhs(), bytecode);
        compile(expr.getOp(), bytecode);
    }
    private void compile(Expr.BOp expr, List<Bytecode> bytecode) {
        switch (expr) {
            case AND:
                break;
            case OR:
                break;
            case ADD:
                break;
            case SUB:
                break;
            case MUL:
                break;
            case DIV:
                break;
            case REM:
                break;
            case EQ:
                break;
            case NEQ:
                break;
            case LT:
                break;
            case LTEQ:
                break;
            case GT:
                break;
            case GTEQ:
                break;
        }
    }
    private void compile(Expr.Constant expr, List<Bytecode> bytecode) {

    }

    private void compile(Expr.IndexOf expr, List<Bytecode> bytecode) {

    }
    private void compile(Expr.RecordAccess expr, List<Bytecode> bytecode) {
        throw new UnsupportedOperationException();
    }
    private void compile(Expr.RecordConstructor expr, List<Bytecode> bytecode) {
        throw new UnsupportedOperationException();
    }
    private void compile(Expr.Unary expr, List<Bytecode> bytecode) {
        compile(expr.getExpr(), bytecode);
        compile(expr.getOp(), bytecode);
    }

    private void compile(Expr.UOp expr, List<Bytecode> bytecode) {
        switch (expr) {

            case NOT:
                break;
            case NEG:
                break;
            case LENGTHOF:
                break;
        }
    }
    private void compile(Expr.Variable expr, List<Bytecode> bytecode) {

    }


    /**
     * Convert type from While to JVM type
     *
     * @param input
     * @return
     */
    private JvmType convertType(Type input) {
        if (input instanceof Type.Void) {
           return JvmTypes.VOID;
        } else if (input instanceof Type.Int) {
            return JvmTypes.INT;
        } else if (input instanceof Type.Bool) {
            return JvmTypes.BOOL;
        } else if (input instanceof Type.Char) {
            return JvmTypes.CHAR;
        } else if (input instanceof Type.Strung) {
            return JvmTypes.JAVA_LANG_STRING;
        } else if (input instanceof Type.Array) {
            Type.Array arrayType = (Type.Array) input;
            return new JvmType.Array(convertType(arrayType.getElement()));
        } else {
            // not implmented
            throw new UnsupportedOperationException();
        }
    }

    public void internalFailure(String msg, SyntacticElement elem) {
        int start = -1;
        int end = -1;

        Attribute.Source attr = (Attribute.Source) elem.attribute(Attribute.Source.class);
        if (attr != null) {
            start = attr.start;
            end = attr.end;
        }

        throw new SyntaxError.InternalFailure(msg, fileName, start, end);
    }

}
