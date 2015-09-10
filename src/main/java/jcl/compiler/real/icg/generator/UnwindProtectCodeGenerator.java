package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class UnwindProtectCodeGenerator extends SpecialOperatorCodeGenerator<UnwindProtectStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private UnwindProtectCodeGenerator() {
		super("unwindProtect");
	}

	@Override
	protected void generateSpecialOperator(final UnwindProtectStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct protectedForm = input.getProtectedForm();
		final PrognStruct cleanupForms = input.getCleanupForms();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);
		codeGenerator.generate(protectedForm, generatorState);
		final int protectedFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, protectedFormStore);

		mv.visitLabel(tryBlockEnd);
		prognCodeGenerator.generate(cleanupForms, generatorState);
		mv.visitInsn(Opcodes.POP);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		prognCodeGenerator.generate(cleanupForms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, protectedFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
