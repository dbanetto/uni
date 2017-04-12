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

    private final WhileFile file;

    public JvmCompiler(WhileFile file) {
        this.file = file;
    }

    public void compile() {
        try {
            JasmFileWriter writer = new JasmFileWriter(new FileOutputStream(file.filename.replace("while", "class")));

            for (WhileFile.Decl decl : file.declarations) {

            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

    }

}
