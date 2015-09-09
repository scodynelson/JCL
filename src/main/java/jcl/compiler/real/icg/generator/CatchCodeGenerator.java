package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class CatchCodeGenerator implements CodeGenerator<CatchStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private static final String CATCH_METHOD_NAME_PREFIX = "catch_";

	private static final String CATCH_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	@Override
	public void generate(final CatchStruct input, final GeneratorState generatorState) {

		final LispStruct catchTag = input.getCatchTag();
		final PrognStruct forms = input.getForms();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String catchMethodName = CATCH_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, catchMethodName, CATCH_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.THROW_EXCEPTION_NAME);

		codeGenerator.generate(catchTag, generatorState);
		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		mv.visitLabel(tryBlockStart);
		prognCodeGenerator.generate(forms, generatorState);
		final int resultFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int throwExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.THROW_EXCEPTION_NAME,
				GenerationConstants.THROW_EXCEPTION_GET_CATCH_TAG_METHOD_NAME,
				GenerationConstants.THROW_EXCEPTION_GET_CATCH_TAG_METHOD_DESC,
				false);
		final int throwExceptionCatchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionCatchTagStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionCatchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_OBJECT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);

		final Label rethrowException = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, rethrowException);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.THROW_EXCEPTION_NAME,
				GenerationConstants.THROW_EXCEPTION_GET_RESULT_FORM_METHOD_NAME,
				GenerationConstants.THROW_EXCEPTION_GET_RESULT_FORM_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(rethrowException);
		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, catchMethodName, CATCH_METHOD_DESC, false);
	}
}
