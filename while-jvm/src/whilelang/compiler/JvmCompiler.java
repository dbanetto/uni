package whilelang.compiler;

import jasm.attributes.Code;
import jasm.io.ClassFileWriter;
import jasm.lang.*;
import whilelang.ast.*;
import whilelang.util.Pair;
import whilelang.util.SyntacticElement;
import whilelang.util.SyntaxError;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;

/**
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
            JvmType.Clazz base = new JvmType.Clazz("", className);

            ClassFile classFile = new ClassFile(49, // java 1.5 or later
                    base,
                    JvmTypes.JAVA_LANG_OBJECT,
                    Collections.EMPTY_LIST,
                    classModifiers
            );

            List<Pair<ClassFile.Method, WhileFile.MethodDecl>> methods = new ArrayList<>();
            Environment env = new Environment(base);
            boolean javaMain = false;

            for (WhileFile.Decl decl : ast.declarations) {

                if (decl instanceof WhileFile.MethodDecl) {
                    WhileFile.MethodDecl mDecl = (WhileFile.MethodDecl) decl;
                    ClassFile.Method method = setupMethod(mDecl, env);

                    env.addMethod(mDecl.getName(), method);
                    methods.add(new Pair<>(method, mDecl));
                    classFile.methods().add(method);

                    if (mDecl.getName().equals("main") && mDecl.getParameters().size() > 0) {
                        javaMain = true;
                    }
                } else if (decl instanceof WhileFile.TypeDecl) {
                    WhileFile.TypeDecl typeDecl = (WhileFile.TypeDecl) decl;
                    env.addType(typeDecl.getName(), typeDecl.getType());
                }
            }

            if (!javaMain) {

                List<Stmt> stmts = new ArrayList<>();
                stmts.add(new Expr.Invoke("main", Collections.EMPTY_LIST));
                WhileFile.MethodDecl mDecl = new WhileFile.MethodDecl("main", new Type.Void(),
                        Arrays.asList(new WhileFile.Parameter(new Type.Array(new Type.Strung()), "args")),
                        stmts
                );

                ClassFile.Method method = setupMethod(mDecl, env);
                env.addMethod(mDecl.getName(), method);

                methods.add(new Pair<>(method, mDecl));
                classFile.methods().add(method);
            }

            for (Pair<ClassFile.Method, WhileFile.MethodDecl> m : methods) {
                env.slotCount = 0;
                compileMethod(classFile, m.first(), m.second(), env);
            }

            classWriter.write(classFile);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private ClassFile.Method setupMethod(WhileFile.MethodDecl methodDecl, Environment env) {

        /// ------ setup method ------
        List<Modifier> funcModifiers = new ArrayList<>();
        funcModifiers.add(Modifier.ACC_STATIC);

        // special case where main is a public method
        if (methodDecl.getName().equals("main")) {
            funcModifiers.add(Modifier.ACC_PUBLIC);
        }

        List<JvmType> parameterTypes = new ArrayList<>(methodDecl.getParameters().size());
        for (WhileFile.Parameter param : methodDecl.getParameters()) {
            parameterTypes.add(convertType(param.getType(), env));
        }

        JvmType returnType = convertType(methodDecl.getRet(), env);
        JvmType.Function function = new JvmType.Function(returnType, parameterTypes);

        ClassFile.Method method = new ClassFile.Method(
                methodDecl.name(),
                function,
                funcModifiers);

        return method;
    }

    /**
     * Generate bytecode and method call for class
     *
     * @param classFile
     * @param methodDecl
     */
    private void compileMethod(ClassFile classFile, ClassFile.Method method, WhileFile.MethodDecl methodDecl, Environment env) {
        // --------- build bytecode of method ---------
        List<Bytecode> bytecode = new ArrayList<>();

        Environment methodEnv = new Environment(env);

        for (WhileFile.Parameter p : methodDecl.getParameters()) {
            methodEnv.addVariable(p.getName(), convertType(p.getType(), env));
        }

        // build
        compile(methodDecl.getBody(), bytecode, methodEnv);

        // special case of void methods not required to have a return
        if (method.type().returnType().equals(JvmTypes.VOID)) {
            compile(new Stmt.Return(null), bytecode, methodEnv);
        }

        method.attributes().add(new Code(bytecode, Collections.EMPTY_LIST, method));
    }

    private void compile(List<Stmt> statements, List<Bytecode> bytecode, Environment env) {
        for (Stmt stmt : statements) {
            compile(stmt, bytecode, env);
        }
    }

    private void compile(Stmt stmt, List<Bytecode> bytecode, Environment env) {
        if (stmt instanceof Stmt.Assert) {
            compile((Stmt.Assert) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.Assign) {
            compile((Stmt.Assign) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.Print) {
            compile((Stmt.Print) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.Return) {
            compile((Stmt.Return) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.Break) {
            compile((Stmt.Break) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.Continue) {
            compile((Stmt.Continue) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.VariableDeclaration) {
            compile((Stmt.VariableDeclaration) stmt, bytecode, env);
        } else if (stmt instanceof Expr.Invoke) {
            compile((Expr.Invoke) stmt, false, bytecode, env);
        } else if (stmt instanceof Stmt.IfElse) {
            compile((Stmt.IfElse) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.For) {
            compile((Stmt.For) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.While) {
            compile((Stmt.While) stmt, bytecode, env);
        } else if (stmt instanceof Stmt.Switch) {
            compile((Stmt.Switch) stmt, bytecode, env);
        } else {
            internalFailure("unknown statement encountered (" + stmt + ")", stmt);
        }
    }

    private void compile(Stmt.VariableDeclaration stmt, List<Bytecode> bytecode, Environment env) {
        JvmType jvmtype = convertType(stmt.getType(), env);

        Variable var = env.addVariable(stmt.getName(), jvmtype);

        if (stmt.getExpr() != null) {
            compile(stmt.getExpr(), bytecode, env);
            bytecode.add(new Bytecode.Store(var.slot, var.jvmtype));
        }
    }

    private void compile(Stmt.Assert stmt, List<Bytecode> bytecode, Environment env) {
        String assertLabel = label("assert");
        JvmType.Clazz assertError = JvmTypes.JAVA_LANG_RUNTIMEEXCEPTION;

        // assertion expression
        compile(stmt.getExpr(), bytecode, env);
        // jump to success if true
        bytecode.add(new Bytecode.If(Bytecode.IfMode.NE, assertLabel));
        // create Assertion Error object
        bytecode.add(new Bytecode.New(assertError));
        // dup so it can be init'd & thrown
        bytecode.add(new Bytecode.Dup(assertError));
        bytecode.add(new Bytecode.Invoke(assertError, "<init>", new JvmType.Function(JvmTypes.VOID, Collections.EMPTY_LIST), Bytecode.InvokeMode.SPECIAL));
        // throw error
        bytecode.add(new Bytecode.Throw());
        // success label
        bytecode.add(new Bytecode.Label(assertLabel));
    }

    private void compile(Stmt.Break stmt, List<Bytecode> bytecode, Environment env) {

        throw new UnsupportedOperationException();
    }

    private void compile(Stmt.Assign stmt, List<Bytecode> bytecode, Environment env) {
        compile(stmt.getRhs(), bytecode, env);

        if (stmt.getLhs() instanceof Expr.Variable) {
            Expr.Variable exprVar = (Expr.Variable) stmt.getLhs();
            Variable var = env.getVariable(exprVar.getName());
            bytecode.add(new Bytecode.Store(var.slot, var.jvmtype));
        } else {
            throw new UnsupportedOperationException();
        }

    }

    private void compile(Stmt.Continue stmt, List<Bytecode> bytecode, Environment env) {
        throw new UnsupportedOperationException();
    }

    private void compile(Stmt.For stmt, List<Bytecode> bytecode, Environment env) {

        throw new UnsupportedOperationException();
    }

    private void compile(Stmt.IfElse stmt, List<Bytecode> bytecode, Environment env) {
        String trueBranch = label("true_branch");
        String endBranch = label("end_branch");
        Environment ifEnv = new Environment(env);
        boolean falseTerminates = allBranchesTerminate(stmt.getFalseBranch());

        compile(stmt.getCondition(), bytecode, env);
        bytecode.add(new Bytecode.If(Bytecode.IfMode.NE, trueBranch));

        compile(stmt.getFalseBranch(), bytecode, ifEnv);
        if (!falseTerminates) {
            bytecode.add(new Bytecode.Goto(endBranch));
        }

        bytecode.add(new Bytecode.Label(trueBranch));
        compile(stmt.getTrueBranch(), bytecode, ifEnv);

        if (!falseTerminates) {
            bytecode.add(new Bytecode.Label(endBranch));
        }
    }

    private void compile(Stmt.Print stmt, List<Bytecode> bytecode, Environment env) {

        throw new UnsupportedOperationException();
    }

    private void compile(Stmt.Return stmt, List<Bytecode> bytecode, Environment env) {
        JvmType returnType = null;
        if (stmt.getExpr() != null) {
            compile(stmt.getExpr(), bytecode, env);
            returnType = convertType(stmt.getExpr(), env);
        }

        bytecode.add(new Bytecode.Return(returnType));
    }

    private void compile(Stmt.Switch stmt, List<Bytecode> bytecode, Environment env) {

        throw new UnsupportedOperationException();
    }

    private void compile(Stmt.Case stmt, List<Bytecode> bytecode, Environment env) {

        throw new UnsupportedOperationException();
    }

    private void compile(Stmt.While stmt, List<Bytecode> bytecode, Environment env) {

        throw new UnsupportedOperationException();
    }

    private void compile(Expr expr, List<Bytecode> bytecode, Environment env) {

        if (expr instanceof Expr.Binary) {
            compile((Expr.Binary) expr, bytecode, env);
        } else if (expr instanceof Expr.Constant) {
            compile((Expr.Constant) expr, bytecode, env);
        } else if (expr instanceof Expr.IndexOf) {
            compile((Expr.IndexOf) expr, bytecode, env);
        } else if (expr instanceof Expr.Invoke) {
            compile((Expr.Invoke) expr, true, bytecode, env);
        } else if (expr instanceof Expr.ArrayGenerator) {
            compile((Expr.ArrayGenerator) expr, bytecode, env);
        } else if (expr instanceof Expr.ArrayInitialiser) {
            compile((Expr.ArrayInitialiser) expr, bytecode, env);
        } else if (expr instanceof Expr.RecordAccess) {
            compile((Expr.RecordAccess) expr, bytecode, env);
        } else if (expr instanceof Expr.RecordConstructor) {
            compile((Expr.RecordConstructor) expr, bytecode, env);
        } else if (expr instanceof Expr.Unary) {
            compile((Expr.Unary) expr, bytecode, env);
        } else if (expr instanceof Expr.Variable) {
            compile((Expr.Variable) expr, bytecode, env);
        } else {
            internalFailure("unknown expression encountered (" + expr + ")", expr);
        }
    }


    private void compile(Expr.Invoke expr, boolean inExpression, List<Bytecode> bytecode, Environment env) {

        for (Expr e : expr.getArguments()) {
           compile(e, bytecode, env);
        }
        // TODO: invoke static function call
        ClassFile.Method m = env.getMethod(expr.getName(), convertType(expr.getArguments(), env));
        bytecode.add(new Bytecode.Invoke(env.getBaseClass(), expr.getName(), m.type(), Bytecode.InvokeMode.STATIC));
    }

    private void compile(Expr.ArrayGenerator expr, List<Bytecode> bytecode, Environment env) {
        throw new UnsupportedOperationException();
    }

    private void compile(Expr.ArrayInitialiser expr, List<Bytecode> bytecode, Environment env) {
        throw new UnsupportedOperationException();
    }

    private void compile(Expr.Binary expr, List<Bytecode> bytecode, Environment env) {
        expr.attributes();
        boolean shortCircuit = expr.getOp() == Expr.BOp.AND || expr.getOp() == Expr.BOp.OR;
        String endLabel = label("short_circuit");

        compile(expr.getLhs(), bytecode, env);
        if (shortCircuit) {
            Bytecode.IfMode ifMode = expr.getOp() == Expr.BOp.AND ? Bytecode.IfMode.EQ : Bytecode.IfMode.NE;
            bytecode.add(new Bytecode.Dup(JvmTypes.BOOL));
            bytecode.add(new Bytecode.If(ifMode, endLabel));
        }
        compile(expr.getRhs(), bytecode, env);

        compile(expr.getOp(), bytecode, env, convertType(expr.getLhs(), env));
        if (shortCircuit) {
            bytecode.add(new Bytecode.Label(endLabel));
        }
    }

    private void compile(Expr.BOp expr, List<Bytecode> bytecode, Environment env, JvmType jvmtype) {
        switch (expr) {
            case AND:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.AND, jvmtype));
                break;
            case OR:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.OR, jvmtype));
                break;

            case ADD:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.ADD, jvmtype));
                break;
            case SUB:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.SUB, jvmtype));
                break;
            case MUL:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.MUL, jvmtype));
                break;
            case DIV:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.DIV, jvmtype));
                break;
            case REM:
                bytecode.add(new Bytecode.BinOp(Bytecode.BinOp.REM, jvmtype));
                break;
            case EQ:
                compare(Bytecode.IfCmp.EQ, bytecode, jvmtype);
                break;
            case NEQ:
                compare(Bytecode.IfCmp.NE, bytecode, jvmtype);
                break;
            case LT:
                compare(Bytecode.IfCmp.LT, bytecode, jvmtype);
                break;
            case LTEQ:
                compare(Bytecode.IfCmp.LE, bytecode, jvmtype);
                break;
            case GT:
                compare(Bytecode.IfCmp.GT, bytecode, jvmtype);
                break;
            case GTEQ:
                compare(Bytecode.IfCmp.GE, bytecode, jvmtype);
                break;
        }
    }

    private void compare(int ifCmp, List<Bytecode> bytecode, JvmType jvmtype) {
        String alt = label("cmp_alt");
        String end = label("cmp_end");

        bytecode.add(new Bytecode.IfCmp(ifCmp, jvmtype, alt));
        bytecode.add(new Bytecode.LoadConst(false));
        bytecode.add(new Bytecode.Goto(end));
        bytecode.add(new Bytecode.Label(alt));
        bytecode.add(new Bytecode.LoadConst(true));
        bytecode.add(new Bytecode.Label(end));
    }

    private void compile(Expr.Constant expr, List<Bytecode> bytecode, Environment env) {
        bytecode.add(new Bytecode.LoadConst(expr.getValue()));
    }

    private void compile(Expr.IndexOf expr, List<Bytecode> bytecode, Environment env) {
        compile(expr.getSource(), bytecode, env);
        compile(expr.getIndex(), bytecode, env);
        JvmType.Array exprType = (JvmType.Array) convertType(expr.getSource(), env);
        bytecode.add(new Bytecode.ArrayLoad(exprType));
    }

    private void compile(Expr.RecordAccess expr, List<Bytecode> bytecode, Environment env) {
        throw new UnsupportedOperationException();
    }

    private void compile(Expr.RecordConstructor expr, List<Bytecode> bytecode, Environment env) {
        throw new UnsupportedOperationException();
    }

    private void compile(Expr.Unary expr, List<Bytecode> bytecode, Environment env) {
        compile(expr.getExpr(), bytecode, env);
        compile(expr.getOp(), bytecode, convertType(expr.getExpr(), env));
    }

    private void compile(Expr.UOp expr, List<Bytecode> bytecode, JvmType jvmtype) {
        switch (expr) {

            case NOT:
                String alt = label("not_alt");
                String end = label("not_end");

                bytecode.add(new Bytecode.If(Bytecode.IfMode.NE, alt, jvmtype instanceof JvmType.Long));
                bytecode.add(new Bytecode.LoadConst(true));
                bytecode.add(new Bytecode.Goto(end));
                bytecode.add(new Bytecode.Label(alt));
                bytecode.add(new Bytecode.LoadConst(false));
                bytecode.add(new Bytecode.Label(end));
                break;
            case NEG:
                bytecode.add(new Bytecode.Neg(jvmtype));
                break;
            case LENGTHOF:
                bytecode.add(new Bytecode.ArrayLength());
                break;
        }
    }

    private void compile(Expr.Variable expr, List<Bytecode> bytecode, Environment env) {
        Variable var = env.getVariable(expr.getName());
        bytecode.add(new Bytecode.Load(var.slot, var.jvmtype));
    }


    private JvmType convertType(Expr expr, Environment env) {
        Attribute.Type type = expr.attribute(Attribute.Type.class);
        if (type != null) {
            return convertType(type.type, env);
        } else {
            throw new IllegalStateException();
        }
    }

    private List<JvmType> convertType(List<Expr> expr, Environment env) {
        List<JvmType> types = new ArrayList<>(expr.size());
        for (Expr e : expr) {
            types.add(convertType(e, env));
        }
        return types;
    }

    /**
     * Convert type from While to JVM type
     *
     * @param input
     * @return
     */
    private JvmType convertType(Type input, Environment env) {
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
            return new JvmType.Array(convertType(arrayType.getElement(), env));
        } else if (input instanceof Type.Named) {
            Type.Named named = (Type.Named) input;
            return convertType(env.getType(named.getName()), env);
        } else {
            // not implmented
            throw new UnsupportedOperationException();
        }
    }

    private boolean allBranchesTerminate(List<Stmt> stmts) {
        boolean terminates = false;
        for (Stmt stmt : stmts) {
            if (stmt instanceof Stmt.IfElse) {
                Stmt.IfElse ifElse = (Stmt.IfElse) stmt;
                boolean falseBranch = allBranchesTerminate(ifElse.getFalseBranch());
                if (allBranchesTerminate(ifElse.getTrueBranch()) && falseBranch) {
                    terminates = true;
                }
            } else if (stmt instanceof Stmt.Return) {
                terminates = true;
            } else {
                terminates = false;
            }
        }
        return terminates;
    }

    public void internalFailure(String msg, SyntacticElement elem) {
        int start = -1;
        int end = -1;

        Attribute.Source attr = elem.attribute(Attribute.Source.class);
        if (attr != null) {
            start = attr.start;
            end = attr.end;
        }

        throw new SyntaxError.InternalFailure(msg, fileName, start, end);
    }

    private int labelNonce = 0;
    private String label(String title) {
        return title + (labelNonce++);
    }

    private static class Variable {
        private final JvmType jvmtype;
        private final int slot;

        public Variable(JvmType jvmtype, int slot) {
            this.jvmtype = jvmtype;
            this.slot = slot;
        }
    }

    private static class Environment {

        private final JvmType.Clazz baseClazz;
        private int slotCount = 0;
        private final Map<String, Variable> variables = new HashMap<>();
        private final Map<String, Type> types = new HashMap<>();
        private final Map<String, List<ClassFile.Method>> methods = new HashMap<>();
        private final Environment parent;

        public Environment(JvmType.Clazz base) {
            this.parent = null;
            this.baseClazz = base;
        }

        public Environment(Environment parent) {
            this.baseClazz = null;
            this.parent = parent;
        }

        public Variable addVariable(String name, JvmType jvmtype) {
            Variable toAdd = new Variable(jvmtype, this.newSlot(jvmtype));
            this.variables.put(name, toAdd);
            return toAdd;
        }

        public Variable getVariable(String name) {
            if (variables.containsKey(name)) {
                return variables.get(name);
            } else if (this.parent != null) {
                return this.parent.getVariable(name);
            }
            throw new IllegalStateException("Unknown variable " + name);
        }

        public ClassFile.Method getMethod(String name, List<JvmType> parameters) {
            if (this.methods.containsKey(name)) {
                for (ClassFile.Method m : this.methods.get(name)) {
                    if (m.name().equals(name) && m.type().parameterTypes().equals(parameters)) {
                        return m;
                    }
                }
            }
            if (this.parent != null) {
                return this.parent.getMethod(name, parameters);
            }
            throw new IllegalStateException("Unknown method " + name);
        }

        public Type getType(String name) {
            if (this.types.containsKey(name)) {
                return this.types.get(name);
            } else if (this.parent != null) {
                return this.parent.getType(name);
            }
            throw new IllegalStateException("Unknown type " + name);
        }

        public void addType(String name, Type type) {
            this.types.put(name, type);
        }

        private int newSlot(JvmType jvmtype) {
            if (this.parent != null) {
                return this.parent.newSlot(jvmtype);
            }
            int allocated = this.slotCount;
            this.slotCount += ClassFile.slotSize(jvmtype);
            return allocated;
        }

        public Environment getRoot() {
            if (this.parent != null) {
                return this.parent.getRoot();
            }
            return this;
        }
        public JvmType.Clazz getBaseClass() {
            if (this.parent != null) {
                return this.parent.getBaseClass();
            }
            return this.baseClazz;
        }

        public void addMethod(String name, ClassFile.Method method) {
            if (this.methods.containsKey(name)) {
                this.methods.get(name).add(method);
            } else {
                this.methods.put(name, new ArrayList<ClassFile.Method>());
                this.methods.get(name).add(method);
            }
        }
    }
}
