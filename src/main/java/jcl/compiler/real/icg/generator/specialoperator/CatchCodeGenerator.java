package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchCodeGenerator implements CodeGenerator<CatchStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final CatchStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "catchGen", "()V", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");

		final Label getCatchTagValue = new Label();
		mv.visitLabel(getCatchTagValue);
//		mv.visitLineNumber(61, getCatchTagValue);
		final LispStruct catchTag = input.getCatchTag();
		formGenerator.generate(catchTag, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		mv.visitLabel(tryBlockStart);
//		mv.visitLineNumber(65, tryBlockStart);
		final List<LispStruct> forms = input.getForms();
		// TODO: would be nice if we just have the SA make this into a PrognStruct...
		for (final Iterator<LispStruct> iterator = forms.iterator(); iterator.hasNext(); ) {

			final LispStruct form = iterator.next();
			if (form == null) {
				// Remove the current element from the iterator and the list.
				iterator.remove();
			}
			formGenerator.generate(form, classBuilder);
			if (iterator.hasNext()) {
				currentClass.getMethodVisitor().visitInsn(Opcodes.POP);
			}
		}
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		mv.visitLabel(tryBlockEnd);
//		mv.visitLineNumber(72, tryBlockEnd);
		final Label catchBlockEnd = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlock);
//		mv.visitLineNumber(66, catchBlock);
		mv.visitFrame(Opcodes.F_FULL, 1, new Object[]{"jcl/LispStruct"}, 1, new Object[]{"jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException"});
		mv.visitVarInsn(Opcodes.ASTORE, 3);

		final Label getThrowCatchTag = new Label();
		mv.visitLabel(getThrowCatchTag);
//		mv.visitLineNumber(67, getThrowCatchTag);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getCatchTag", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 4);

		final Label checkEqualsThrowCatchTagAndCatchTag = new Label();
		mv.visitLabel(checkEqualsThrowCatchTagAndCatchTag);
//		mv.visitLineNumber(68, checkEqualsThrowCatchTagAndCatchTag);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);

		final Label setResultValue = new Label();
		mv.visitJumpInsn(Opcodes.IFNE, setResultValue);

		final Label rethrowThrowException = new Label();
		mv.visitLabel(rethrowThrowException);
//		mv.visitLineNumber(69, rethrowThrowException);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(setResultValue);
//		mv.visitLineNumber(71, setResultValue);
		mv.visitFrame(Opcodes.F_APPEND, 3, new Object[]{Opcodes.TOP, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "jcl/LispStruct"}, 0, null);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getResultForm", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		mv.visitLabel(catchBlockEnd);
//		mv.visitLineNumber(73, catchBlockEnd);
		mv.visitFrame(Opcodes.F_FULL, 2, new Object[]{"jcl/LispStruct", "jcl/LispStruct"}, 0, new Object[]{});
		mv.visitVarInsn(Opcodes.ALOAD, 2);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("catchTag", "Ljcl/LispStruct;", null, tryBlockStart, localVariables, 1);
		mv.visitLocalVariable("resultForm", "Ljcl/LispStruct;", null, tryBlockEnd, catchBlock, 2);
		mv.visitLocalVariable("resultForm", "Ljcl/LispStruct;", null, catchBlockEnd, localVariables, 2);
		mv.visitLocalVariable("te", "Ljcl/compiler/real/icg/generator/specialoperator/exception/ThrowException;", null, getThrowCatchTag, catchBlockEnd, 3);
		mv.visitLocalVariable("teCatchTag", "Ljcl/LispStruct;", null, checkEqualsThrowCatchTagAndCatchTag, catchBlockEnd, 4);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 4);
		mv.visitEnd();
	}
}
