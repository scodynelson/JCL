package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
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

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, null);

		mv.visitLabel(tryBlockStart);
		final LispStruct protectedForm = input.getProtectedForm();
		formGenerator.generate(protectedForm, classBuilder);
		final int protectedFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, protectedFormStore);

		mv.visitLabel(tryBlockEnd);
		final PrognStruct cleanupForms = input.getCleanupForms();
		prognCodeGenerator.generate(cleanupForms, classBuilder);
		mv.visitInsn(Opcodes.POP);
		mv.visitVarInsn(Opcodes.ALOAD, protectedFormStore);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
//		mv.visitInsn(Opcodes.ARETURN);

		mv.visitLabel(catchBlock);
		final int exceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);
		prognCodeGenerator.generate(cleanupForms, classBuilder);
		mv.visitInsn(Opcodes.POP);
		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);
	}
}
