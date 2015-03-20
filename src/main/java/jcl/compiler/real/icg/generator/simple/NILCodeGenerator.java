package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.NILStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class NILCodeGenerator implements CodeGenerator<NILStruct> {

	@Override
	public void generate(final NILStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "nilGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label getNILStruct = new Label();
		mv.visitLabel(getNILStruct);
//		mv.visitLineNumber(97, getNILStruct);
		mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(1, 0);
		mv.visitEnd();
	}
}
