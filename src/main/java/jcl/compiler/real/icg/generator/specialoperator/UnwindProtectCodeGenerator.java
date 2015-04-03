package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UnwindProtectCodeGenerator implements CodeGenerator<UnwindProtectStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final UnwindProtectStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct protectedForm = input.getProtectedForm();
		final PrognStruct cleanupForms = input.getCleanupForms();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();
		final MethodVisitor previousMv = currentClass.getMethodVisitor();

		final String unwindProtectMethodName = "unwindProtect_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, unwindProtectMethodName, "()Ljcl/LispStruct;", null, null);
		currentClass.setMethodVisitor(mv);
		mv.visitCode();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);
		formGenerator.generate(protectedForm, classBuilder);
		final int protectedFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, protectedFormStore);

		mv.visitLabel(tryBlockEnd);
		prognCodeGenerator.generate(cleanupForms, classBuilder);
		mv.visitInsn(Opcodes.POP);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		prognCodeGenerator.generate(cleanupForms, classBuilder);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, protectedFormStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		currentClass.setMethodVisitor(previousMv);

		previousMv.visitVarInsn(Opcodes.ALOAD, 0);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, unwindProtectMethodName, "()Ljcl/LispStruct;", false);
	}
}
