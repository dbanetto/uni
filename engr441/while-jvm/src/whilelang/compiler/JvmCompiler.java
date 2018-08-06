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
 * Translate the While AST to Java Bytecode using the JASM library
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

            // pre-fetching of all declarations so all methods can call
            // other methods
            for (WhileFile.Decl decl : ast.declarations) {

                if (decl instanceof WhileFile.MethodDecl) {
                    WhileFile.MethodDecl mDecl = (WhileFile.MethodDecl) decl;
                    ClassFile.Method method = setupMethod(mDecl, env);

                    env.addMethod(mDecl.getName(), method);
                    methods.add(new Pair<>(method, mDecl));
                    classFile.methods().add(method);

                    // checking for a java main method like: main(String[])
                    if (mDecl.getName().equals("main") && mDecl.getParameters().size() > 0) {
                        javaMain = true;
                    }
                } else if (decl instanceof WhileFile.TypeDecl) {
                    WhileFile.TypeDecl typeDecl = (WhileFile.TypeDecl) decl;
                    env.addType(typeDecl.getName(), typeDecl.getType());
                }
            }

            // create a main method for java compatibility
            if (!javaMain) {

                List<Stmt> stmts = new ArrayList<>();

                stmts.add(new Expr.Invoke("main", Collections.EMPTY_LIST));
                WhileFile.MethodDecl mDecl = new WhileFile.MethodDecl("main", new Type.Void(),
                        Arrays.asList(new WhileFile.Parameter(new Type.Array(new Type.Strung()), "args")),
                        stmts
                );

                ClassFile.Method method = setupMethod(mDecl, env);
                // hack to ensure that parameter is String[] instead of ArrayList<String>
                method.type().parameterTypes().remove(0);
                method.type().parameterTypes().add(new JvmType.Array(JvmTypes.JAVA_LANG_STRING));
                env.addMethod(mDecl.getName(), method);

                methods.add(new Pair<>(method, mDecl));
                classFile.methods().add(method);
            }


            for (Pair<ClassFile.Method, WhileFile.MethodDecl> m : methods) {
                // reset the slot counter for each method
                // since each method have their own set of slots
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

        // doesn't matter that the slot is not used right away
        // since the DefiniteAssignment analysis has passed thus it will
        // be set before reading from the slot
        if (stmt.getExpr() != null) {
            compile(stmt.getExpr(), bytecode, env);
            cloneAsNecessary(var.jvmtype, bytecode);
            bytecode.add(new Bytecode.Store(var.slot, var.jvmtype));
        }
    }

    private void compile(Stmt.Assert stmt, List<Bytecode> bytecode, Environment env) {
        String assertLabel = label("assert");
        JvmType.Clazz assertError = new JvmType.Clazz("java.lang","AssertionError");

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
        bytecode.add(new Bytecode.Goto(env.getBreakLabel()));
    }

    private void compile(Stmt.Continue stmt, List<Bytecode> bytecode, Environment env) {
        bytecode.add(new Bytecode.Goto(env.getContinueLabel()));
    }

    private void compile(Stmt.Assign stmt, List<Bytecode> bytecode, Environment env) {

        JvmType rhsType  = convertType(stmt.getRhs(), env);

        // handle different cases of what is being assigned
        if (stmt.getLhs() instanceof Expr.Variable) {
            // straight into a normal variable, just updating a slot
            Expr.Variable exprVar = (Expr.Variable) stmt.getLhs();
            Variable var = env.getVariable(exprVar.getName());

            compile(stmt.getRhs(), bytecode, env);
            cloneAsNecessary(rhsType, bytecode);

            bytecode.add(new Bytecode.Store(var.slot, var.jvmtype));
        } else if (stmt.getLhs() instanceof Expr.IndexOf) {
            // updating an index in an array
            // have to set the value in the array list
            Expr.IndexOf exprIndexOf = (Expr.IndexOf) stmt.getLhs();
            JvmType.Function setMethod = new JvmType.Function(JvmTypes.JAVA_LANG_OBJECT, JvmTypes.INT, JvmTypes.JAVA_LANG_OBJECT);

            compile(exprIndexOf.getSource(), bytecode, env);
            compile(exprIndexOf.getIndex(), bytecode, env);
            compile(stmt.getRhs(), bytecode, env);
            cloneAsNecessary(rhsType, bytecode);
            boxAsNecessary(rhsType, bytecode);

            bytecode.add(new Bytecode.Invoke(JAVA_UTIL_ARRAYLIST, "set", setMethod, Bytecode.InvokeMode.VIRTUAL));
            // pop the removed element from the stack
            bytecode.add(new Bytecode.Pop(JvmTypes.JAVA_LANG_OBJECT));
        } else if (stmt.getLhs() instanceof Expr.RecordAccess) {
            // update a value in a record
            // have to set the value in the hash map
            Expr.RecordAccess exprAccess = (Expr.RecordAccess) stmt.getLhs();
            JvmType.Function putMethod = new JvmType.Function(JvmTypes.JAVA_LANG_OBJECT,JvmTypes.JAVA_LANG_OBJECT,JvmTypes.JAVA_LANG_OBJECT);

            compile(exprAccess.getSource(), bytecode, env);
            bytecode.add(new Bytecode.LoadConst(exprAccess.getName()));

            compile(stmt.getRhs(), bytecode, env);
            boxAsNecessary(rhsType, bytecode);
            cloneAsNecessary(rhsType, bytecode);

            bytecode.add(new Bytecode.Invoke(JAVA_UTIL_HASHMAP, "put", putMethod, Bytecode.InvokeMode.VIRTUAL));
            // pop the old value from the stack
            bytecode.add(new Bytecode.Pop(JvmTypes.JAVA_LANG_OBJECT));
        } else {
            throw new UnsupportedOperationException();
        }

    }

    private void compile(Stmt.For stmt, List<Bytecode> bytecode, Environment env) {

        String breakLabel = label("exit");
        String continueLabel = label("loop_back");
        String conditionLabel = label("condition");

        Environment forEnv = new Environment(env, continueLabel, breakLabel);

        // for's variable declaration
        compile(stmt.getDeclaration(), bytecode, forEnv);
        // skip increment for 1st iteration
        bytecode.add(new Bytecode.Goto(conditionLabel));
        // target to jump back to for continues & starting the loop again
        bytecode.add(new Bytecode.Label(forEnv.getContinueLabel()));
        // apply the increment
        compile(stmt.getIncrement(), bytecode, forEnv);
        // condition jump for 1st iteration
        bytecode.add(new Bytecode.Label(conditionLabel));
        // build the condition
        compile(stmt.getCondition(), bytecode, forEnv);
        // check the condition or go to the end
        bytecode.add(new Bytecode.If(Bytecode.IfMode.EQ, forEnv.getBreakLabel()));
        // build the body of the for loop
        compile(stmt.getBody(), bytecode, forEnv);
        // go back to the start
        bytecode.add(new Bytecode.Goto(forEnv.getContinueLabel()));
        // target to exit the for loop
        bytecode.add(new Bytecode.Label(forEnv.getBreakLabel()));

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

        JvmType.Clazz printStream = new JvmType.Clazz("java.io", "PrintStream");
        JvmType.Clazz system = new JvmType.Clazz("java.lang", "System");
        JvmType exprType = convertType(stmt.getExpr(), env);
        JvmType.Function printlnMethod = new JvmType.Function(JvmTypes.VOID, exprType);

        bytecode.add(new Bytecode.GetField(system, "out", printStream, Bytecode.FieldMode.STATIC));

        compile(stmt.getExpr(), bytecode, env);

        bytecode.add(new Bytecode.Invoke(printStream, "println", printlnMethod, Bytecode.InvokeMode.VIRTUAL));
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

        String exitLabel = label("switch_exit");
        List<Pair<String, Stmt.Case>> cases = new ArrayList<>();
        boolean hasDefault = false;

        Environment caseEnv = new Environment(env, null, exitLabel);
        JvmType exprType = convertType(stmt.getExpr(), env);
        Variable var = env.addVariable("_switch_anon" + exitLabel, exprType);

        compile(stmt.getExpr(), bytecode, env);
        bytecode.add(new Bytecode.Store(var.slot, var.jvmtype));

        // translate the jump table for the switch cases
        for (Stmt.Case c : stmt.getCases()) {
            String caseLabel = label("case_" + (c.isDefault() ? "default" : c.getValue()));

            if (!c.isDefault()) {
                compile(c.getValue(), bytecode, env);
                bytecode.add(new Bytecode.Load(var.slot, var.jvmtype));

                // handle the equals of classes & primitives
                if (exprType instanceof JvmType.Clazz) {
                    JvmType.Function equalsMethod = new JvmType.Function(JvmTypes.BOOL, JvmTypes.JAVA_LANG_OBJECT);
                    bytecode.add(new Bytecode.Invoke((JvmType.Reference)exprType, "equals", equalsMethod, Bytecode.InvokeMode.VIRTUAL));
                    bytecode.add(new Bytecode.If(Bytecode.IfMode.NE, caseLabel));
                } else {
                    bytecode.add(new Bytecode.IfCmp(Bytecode.IfCmp.EQ, exprType, caseLabel));
                }

            } else {
                bytecode.add(new Bytecode.Goto(caseLabel));

                hasDefault = true;
            }
            cases.add(new Pair<>(caseLabel, c));
        }

        if (!hasDefault) {
            bytecode.add(new Bytecode.Goto(exitLabel));
        }


        // translate the cases
        for (Pair<String, Stmt.Case> casePair : cases) {
            bytecode.add(new Bytecode.Label(casePair.first()));
            compile(casePair.second(), bytecode, caseEnv);
        }

        bytecode.add(new Bytecode.Label(exitLabel));
        // noop to ensure  that the exit label always has a target
        // since it is pretty hard to check if all cases return or terminate
        // since they can drop to the next case.
        bytecode.add(new Bytecode.Nop());
    }

    private void compile(Stmt.Case stmt, List<Bytecode> bytecode, Environment env) {
        compile(stmt.getBody(), bytecode, env);
    }

    private void compile(Stmt.While stmt, List<Bytecode> bytecode, Environment env) {

        String breakLabel = label("break_while");
        String continueLabel = label("continue_while");

        Environment whileEnv = new Environment(env, continueLabel, breakLabel);

        // start point of while loop
        bytecode.add(new Bytecode.Label(continueLabel));
        compile(stmt.getCondition(), bytecode, whileEnv);
        bytecode.add(new Bytecode.If(Bytecode.IfMode.EQ, breakLabel));
        compile(stmt.getBody(), bytecode, whileEnv);
        bytecode.add(new Bytecode.Goto(continueLabel));
        // exit label of loop
        bytecode.add(new Bytecode.Label(breakLabel));
        // noop to ensure that the break label always has a target
        bytecode.add(new Bytecode.Nop());
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
           cloneAsNecessary(convertType(e, env), bytecode);
        }

        ClassFile.Method m = env.getMethod(expr.getName(), convertType(expr.getArguments(), env));
        bytecode.add(new Bytecode.Invoke(env.getBaseClass(), expr.getName(), m.type(), Bytecode.InvokeMode.STATIC));
    }

    private void compile(Expr.ArrayGenerator expr, List<Bytecode> bytecode, Environment env) {

        // create an Array List
        JvmType.Function addMethod = new JvmType.Function(JvmTypes.BOOL, JvmTypes.JAVA_LANG_OBJECT);
        Environment genEnv = new Environment(env);
        String startLabel = label("generator_start");
        String endLabel = label("generator_end");

        JvmType valueType = convertType(expr.getValue(), env);
        Variable count = genEnv.addVariable("__generator_count", JvmTypes.INT);
        Variable value = genEnv.addVariable("__generator_value", valueType);

        compile(expr.getSize(), bytecode, env);
        bytecode.add(new Bytecode.Store(count.slot, count.jvmtype));

        compile(expr.getValue(), bytecode, env);
        bytecode.add(new Bytecode.Store(value.slot, value.jvmtype));

        // create new instance of array list
        bytecode.add(new Bytecode.New(JAVA_UTIL_ARRAYLIST));
        // dup so it can be init'd & used
        bytecode.add(new Bytecode.Dup(JAVA_UTIL_ARRAYLIST));
        // call the constructor
        bytecode.add(new Bytecode.Invoke(JAVA_UTIL_ARRAYLIST, "<init>", new JvmType.Function(JvmTypes.VOID), Bytecode.InvokeMode.SPECIAL));

        // loop until all values are generated
        bytecode.add(new Bytecode.Label(startLabel));
        bytecode.add(new Bytecode.Load(count.slot, count.jvmtype));
        bytecode.add(new Bytecode.If(Bytecode.IfMode.EQ, endLabel));

        // duplicate the generated value
        bytecode.add(new Bytecode.Dup(JAVA_UTIL_ARRAYLIST));
        bytecode.add(new Bytecode.Load(value.slot, value.jvmtype));
        boxAsNecessary(valueType, bytecode);
        cloneAsNecessary(valueType, bytecode);

        bytecode.add(new Bytecode.Invoke(JAVA_UTIL_ARRAYLIST, "add", addMethod, Bytecode.InvokeMode.VIRTUAL));
        // discard add's result
        bytecode.add(new Bytecode.Pop(JvmTypes.BOOL));

        bytecode.add(new Bytecode.Iinc(count.slot, -1));

        bytecode.add(new Bytecode.Goto(startLabel));
        bytecode.add(new Bytecode.Label(endLabel));
    }

    private void compile(Expr.ArrayInitialiser expr, List<Bytecode> bytecode, Environment env) {

        // create an Array List
        JvmType.Clazz arrayList = JAVA_UTIL_ARRAYLIST;
        JvmType.Function addMethod = new JvmType.Function(JvmTypes.BOOL, JvmTypes.JAVA_LANG_OBJECT);

        // create new instance of array list
        bytecode.add(new Bytecode.New(arrayList));
        // dup so it can be init'd & used
        bytecode.add(new Bytecode.Dup(arrayList));
        // call the constructor
        bytecode.add(new Bytecode.Invoke(arrayList, "<init>", new JvmType.Function(JvmTypes.VOID), Bytecode.InvokeMode.SPECIAL));

        // put all the values into the array list
        for (Expr e : expr.getArguments()) {
            JvmType eleType = convertType(e, env);
            bytecode.add(new Bytecode.Dup(arrayList));

            compile(e, bytecode, env);
            // box if primitive
            boxAsNecessary(eleType, bytecode);
            cloneAsNecessary(eleType, bytecode);

            bytecode.add(new Bytecode.Invoke(arrayList, "add", addMethod, Bytecode.InvokeMode.VIRTUAL));
            bytecode.add(new Bytecode.Pop(JvmTypes.BOOL));
        }
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

        // handle the case of comparing objects
        if (jvmtype instanceof JvmType.Clazz) {
            JvmType.Function equalsMethod = new JvmType.Function(JvmTypes.BOOL, JvmTypes.JAVA_LANG_OBJECT);
            Bytecode.IfMode ifMode;

            switch (ifCmp) {
                case (Bytecode.IfCmp.EQ):
                    ifMode = Bytecode.IfMode.NE;
                    break;
                case (Bytecode.IfCmp.NE):
                    ifMode = Bytecode.IfMode.EQ;
                    break;
                default:
                    throw new UnsupportedOperationException();
            }

            bytecode.add(new Bytecode.Invoke((JvmType.Reference)jvmtype, "equals", equalsMethod, Bytecode.InvokeMode.VIRTUAL));
            bytecode.add(new Bytecode.If(ifMode, alt));
        } else {
            bytecode.add(new Bytecode.IfCmp(ifCmp, jvmtype, alt));
        }

        bytecode.add(new Bytecode.LoadConst(false));
        bytecode.add(new Bytecode.Goto(end));
        bytecode.add(new Bytecode.Label(alt));
        bytecode.add(new Bytecode.LoadConst(true));
        bytecode.add(new Bytecode.Label(end));
    }

    private void compile(Expr.Constant expr, List<Bytecode> bytecode, Environment env) {
        // handle the cases of constant arrays or records by initializing them
        // this is offloaded to their compile(ArrayInit | RecordConstruct) experssions
        if (expr.getValue() instanceof ArrayList) {
            // Offload to Array Initialiser
            Attribute.Type exprType = expr.attribute(Attribute.Type.class);
            Type.Array arrayType = (Type.Array) exprType.type;

            List<Expr> values = new ArrayList<>();
            ArrayList exprs = (ArrayList) expr.getValue();
            for (Object obj : exprs) {
                values.add(new Expr.Constant(obj, new Attribute.Type(arrayType.getElement())));
            }
            compile(new Expr.ArrayInitialiser(values), bytecode, env);
        } else if (expr.getValue() instanceof HashMap) {
            // offload to Record Constructor
            Attribute.Type exprType = expr.attribute(Attribute.Type.class);
            Type.Record recordType = (Type.Record) exprType.type;

            HashMap exprMap = (HashMap) expr.getValue();
            List<Pair<String, Expr>> fields = new ArrayList<>();
            for (Object key : exprMap.keySet()) {
                Type fieldType = null;
                for (Pair<Type, String> fieldPair : recordType.getFields()) {
                    if (fieldPair.second().equals(key)) {
                        fieldType = fieldPair.first();
                    }
                }

                fields.add(new Pair<>((String)key, new Expr.Constant(exprMap.get(key), new Attribute.Type(fieldType))));
            }

            compile(new Expr.RecordConstructor(fields), bytecode, env);

        } else {
            bytecode.add(new Bytecode.LoadConst(expr.getValue()));
        }
    }

    private void compile(Expr.IndexOf expr, List<Bytecode> bytecode, Environment env) {
        JvmType.Clazz arrayType = (JvmType.Clazz) convertType(expr.getSource(), env);
        JvmType.Function getMethod = new JvmType.Function(JvmTypes.JAVA_LANG_OBJECT, JvmTypes.INT);
        JvmType target = convertType(expr, env);

        compile(expr.getSource(), bytecode, env);
        compile(expr.getIndex(), bytecode, env);
        bytecode.add(new Bytecode.Invoke(arrayType, "get", getMethod, Bytecode.InvokeMode.VIRTUAL));
        if (!unBoxAsNecessary(target, bytecode)) {
            bytecode.add(new Bytecode.CheckCast(target));
        }
    }

    private void compile(Expr.RecordAccess expr, List<Bytecode> bytecode, Environment env) {

        JvmType accessType = convertType(expr, env);
        JvmType.Function getMethod = new JvmType.Function(JvmTypes.JAVA_LANG_OBJECT, JvmTypes.JAVA_LANG_OBJECT);

        compile(expr.getSource(), bytecode, env);
        bytecode.add(new Bytecode.LoadConst(expr.getName()));

        bytecode.add(new Bytecode.Invoke(JAVA_UTIL_HASHMAP, "get", getMethod, Bytecode.InvokeMode.VIRTUAL));
        unBoxAsNecessary(accessType, bytecode);

        if (accessType instanceof JvmType.Clazz) {
            bytecode.add(new Bytecode.CheckCast(accessType));
        }
    }

    private void compile(Expr.RecordConstructor expr, List<Bytecode> bytecode, Environment env) {

        JvmType.Function putMethod = new JvmType.Function(JvmTypes.JAVA_LANG_OBJECT, JvmTypes.JAVA_LANG_OBJECT, JvmTypes.JAVA_LANG_OBJECT);

        // create new instance of hashmap
        // using a hash map as it reflects the width sub-typing of records
        bytecode.add(new Bytecode.New(JAVA_UTIL_HASHMAP));
        // dup so it can be init'd & used
        bytecode.add(new Bytecode.Dup(JAVA_UTIL_HASHMAP));
        // call the constructor
        bytecode.add(new Bytecode.Invoke(JAVA_UTIL_HASHMAP, "<init>", new JvmType.Function(JvmTypes.VOID), Bytecode.InvokeMode.SPECIAL));

        // put each field into the hash map
        for (Pair<String, Expr> field :  expr.getFields()) {
            JvmType fieldType = convertType(field.second(), env);

            bytecode.add(new Bytecode.Dup(JAVA_UTIL_HASHMAP));

            bytecode.add(new Bytecode.LoadConst(field.first()));
            compile(field.second(), bytecode, env);
            boxAsNecessary(fieldType, bytecode);
            cloneAsNecessary(fieldType, bytecode);

            bytecode.add(new Bytecode.Invoke(JAVA_UTIL_HASHMAP, "put", putMethod, Bytecode.InvokeMode.VIRTUAL));
            bytecode.add(new Bytecode.Pop(JvmTypes.JAVA_LANG_OBJECT));
        }

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
                JvmType.Function sizeMethod = new JvmType.Function(JvmTypes.INT);
                bytecode.add(new Bytecode.Invoke(JAVA_UTIL_ARRAYLIST, "size", sizeMethod, Bytecode.InvokeMode.VIRTUAL));
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
            return JAVA_UTIL_ARRAYLIST;
        } else if (input instanceof Type.Named) {
            Type.Named named = (Type.Named) input;
            return convertType(env.getType(named.getName()), env);
        } else if (input instanceof Type.Record) {
            return JAVA_UTIL_HASHMAP;
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
                if (allBranchesTerminate(ifElse.getTrueBranch()) && allBranchesTerminate(ifElse.getFalseBranch())) {
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
        return  title + (labelNonce++);
    }

    private static class Variable {
        private final JvmType jvmtype;
        private final int slot;

        public Variable(JvmType jvmtype, int slot) {
            this.jvmtype = jvmtype;
            this.slot = slot;
        }
    }

    /**
     * Holds the current environment which is being translated
     *
     * Handles all of the methods, types and variables in scope.
     *
     * Break & continue labels are handled by environments to allow for
     * correct nesting of these interrupts.
     *
     * Scope is handled by making Environments a tree structure with
     * only a relation from child to parent.
     */
    private static class Environment {

        private final JvmType.Clazz baseClazz;
        private int slotCount = 0;
        private final Map<String, Variable> variables = new HashMap<>();
        private final Map<String, Type> types = new HashMap<>();
        private final Map<String, List<ClassFile.Method>> methods = new HashMap<>();
        private final Environment parent;
        private final String continueLabel;
        private final String breakLabel;

        public Environment(JvmType.Clazz base) {
            this.parent = null;
            this.baseClazz = base;
            this.continueLabel = null;
            this.breakLabel = null;
        }

        public Environment(Environment parent) {
            this.baseClazz = null;
            this.parent = parent;
            this.continueLabel = null;
            this.breakLabel = null;
        }

        public Environment(Environment parent, String continueLabel, String breakLabel) {
            this.baseClazz = null;
            this.parent = parent;
            this.continueLabel = continueLabel;
            this.breakLabel = breakLabel;
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

        public String getBreakLabel() {
            if (this.breakLabel != null) {
                return this.breakLabel;
            } else if (this.parent != null) {
                return this.parent.getBreakLabel();
            }
            throw new IllegalStateException("No break label in this scope");
        }

        public String getContinueLabel() {
            if (this.continueLabel != null) {
                return this.continueLabel;
            } else if (this.parent != null) {
                return this.parent.getContinueLabel();
            }
            throw new IllegalStateException("No break label in this scope");
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


   	/**
	 * Box the element on top of the stack, if it is of an appropriate type
	 * (i.e. is not a primitive).
	 *
	 * @param jvmType
	 *            The type of the element we are converting from (i.e. on the
	 *            top of the stack).
	 * @param bytecodes
	 *            The list of bytecodes being accumulated
	 */
	private void boxAsNecessary(JvmType jvmType, List<Bytecode> bytecodes) {
		JvmType.Clazz owner;

		if(jvmType instanceof JvmType.Reference) {
			// Only need to box primitive types
			return;
		} else if(jvmType instanceof JvmType.Bool) {
			owner = JvmTypes.JAVA_LANG_BOOLEAN;
		} else if(jvmType instanceof JvmType.Char) {
			owner = JvmTypes.JAVA_LANG_CHARACTER;
		} else if(jvmType instanceof JvmType.Int) {
			owner = JvmTypes.JAVA_LANG_INTEGER;
		} else {
			throw new IllegalArgumentException("unknown primitive type encountered: " + jvmType);
		}

		String boxMethodName = "valueOf";
		JvmType.Function boxMethodType = new JvmType.Function(owner,jvmType);
		bytecodes.add(new Bytecode.Invoke(owner, boxMethodName, boxMethodType,
				Bytecode.InvokeMode.STATIC));
	}

	/**
	 * Unbox a reference type when appropriate. That is, when it represented a
	 * boxed primitive type.
	 *
	 * @param target
	 * @param bytecodes
	 */
	private boolean unBoxAsNecessary(JvmType target, List<Bytecode> bytecodes) {
		String unBoxMethodName;
		JvmType.Reference boxedJvmType;

		if (target instanceof JvmType.Bool) {
			unBoxMethodName = "booleanValue";
			boxedJvmType = JvmTypes.JAVA_LANG_BOOLEAN;
		} else if (target instanceof JvmType.Char) {
			unBoxMethodName = "charValue";
			boxedJvmType = JvmTypes.JAVA_LANG_CHARACTER;
		} else if (target instanceof JvmType.Int) {
			unBoxMethodName = "intValue";
			boxedJvmType = JvmTypes.JAVA_LANG_INTEGER;
		} else {
			return false; // not necessary to unbox
		}
        JvmType.Function unBoxMethodType = new JvmType.Function(target);

        bytecodes.add(new Bytecode.CheckCast(boxedJvmType));
		bytecodes.add(new Bytecode.Invoke(boxedJvmType, unBoxMethodName, unBoxMethodType, Bytecode.InvokeMode.VIRTUAL));
		return true;
	}

	/**
	 * Clone the element on top of the stack, if it is of an appropriate type
	 * (i.e. is not a primitive).
	 *
	 * @param type
	 *            The type of the element on the top of the stack.
	 * @param bytecodes
	 *            The list of bytecodes being accumulated
	 */
	private void cloneAsNecessary(JvmType type, List<Bytecode> bytecodes) {
		if(type instanceof JvmType.Primitive || type == JvmTypes.JAVA_LANG_STRING) {
			// no need to do anything in the case of a primitive type
		} else {
			// Invoke the clone function on the datatype in question
			JvmType.Function ft = new JvmType.Function(JvmTypes.JAVA_LANG_OBJECT);
			bytecodes.add(new Bytecode.Invoke((JvmType.Reference) type, "clone", ft, Bytecode.InvokeMode.VIRTUAL));
			bytecodes.add(new Bytecode.CheckCast(type));
		}
	}

	// A few helpful constants not defined in JvmTypes
	private static final JvmType.Clazz JAVA_UTIL_LIST = new JvmType.Clazz("java.util","List");
	private static final JvmType.Clazz JAVA_UTIL_ARRAYLIST = new JvmType.Clazz("java.util","ArrayList");
	private static final JvmType.Clazz JAVA_UTIL_HASHMAP = new JvmType.Clazz("java.util","HashMap");
	private static final JvmType.Clazz JAVA_UTIL_COLLECTION = new JvmType.Clazz("java.util","Collection");
	private static final JvmType.Clazz JAVA_UTIL_COLLECTIONS = new JvmType.Clazz("java.util","Collections");

	private JvmType.Reference toBoxedJvmType(Type t, Environment env) {
		if(t instanceof Type.Bool) {
			return JvmTypes.JAVA_LANG_BOOLEAN;
		} else if(t instanceof Type.Char) {
			return JvmTypes.JAVA_LANG_CHARACTER;
		} else if(t instanceof Type.Int) {
			return JvmTypes.JAVA_LANG_INTEGER;
		} else if(t instanceof Type.Strung) {
			return JvmTypes.JAVA_LANG_STRING;
		} else if(t instanceof Type.Named) {
			Type.Named d = (Type.Named) t;
			return toBoxedJvmType(env.getType(d.getName()), env);
		} else if(t instanceof Type.Array) {
			return JAVA_UTIL_ARRAYLIST;
		} else if(t instanceof Type.Record) {
			return JAVA_UTIL_HASHMAP;
		} else {
			throw new IllegalArgumentException("Unknown type encountered: " + t);
		}
	}
}
