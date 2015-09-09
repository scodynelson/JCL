package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class ThrowCodeGenerator implements CodeGenerator<ThrowStruct> {

	@Autowired
	private FormGenerator formGenerator;

	private static final String THROW_METHOD_NAME_PREFIX = "throw_";

	private static final String THROW_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private static final String THROW_EXCEPTION_INIT_DESC = GeneratorUtils.getConstructorDescription(ThrowException.class, LispStruct.class, LispStruct.class);

	@Override
	public void generate(final ThrowStruct input, final GeneratorState generatorState) {

		final LispStruct catchTag = input.getCatchTag();
		final LispStruct resultForm = input.getResultForm();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String throwMethodName = THROW_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, throwMethodName, THROW_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		formGenerator.generate(catchTag, generatorState);
		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		formGenerator.generate(resultForm, generatorState);
		final int resultFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.THROW_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.THROW_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				THROW_EXCEPTION_INIT_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, throwMethodName, THROW_METHOD_DESC, false);
	}
}