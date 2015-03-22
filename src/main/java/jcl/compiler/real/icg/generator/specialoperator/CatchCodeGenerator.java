package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchCodeGenerator implements CodeGenerator<CatchStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final CatchStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");

		final LispStruct catchTag = input.getCatchTag();
		formGenerator.generate(catchTag, classBuilder);
		final int catchTagStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		mv.visitLabel(tryBlockStart);
		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, classBuilder);
		final int resultFormStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlock);
		final int throwExceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getCatchTag", "()Ljcl/LispStruct;", false);
		final int throwExceptionCatchTagStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionCatchTagStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionCatchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);

		final Label setResultValue = new Label();
		mv.visitJumpInsn(Opcodes.IFNE, setResultValue);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(setResultValue);
		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getResultForm", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
//		mv.visitInsn(Opcodes.ARETURN);
	}
}
