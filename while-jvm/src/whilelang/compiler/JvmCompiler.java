package whilelang.compiler;

import jasm.io.ClassFileWriter;
import jasm.io.JasmFileWriter;
import jasm.lang.ClassFile;
import whilelang.ast.WhileFile;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;

/**
 *
 * @author David Barnett
 */
public class JvmCompiler {

    private final String fileName;

    public JvmCompiler(String fileName) {
        this.fileName = fileName;
    }

    public void write(WhileFile ast) {
        try {
            JasmFileWriter writer = new JasmFileWriter(new FileOutputStream(fileName));

            for (WhileFile.Decl decl : ast.declarations) {

            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

    }

}
