package jcl.compiler.icg.generator;

import java.util.List;

import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.JavaMethodCallStruct;
import jcl.lang.LispStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'java-method-call' special operator code generation.
 */
@Component
final class JavaMethodCallCodeGenerator extends SpecialOperatorCodeGenerator<JavaMethodCallStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link JavaMethodCallStruct#javaObject} and {@link JavaMethodCallStruct#arguments}.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Private constructor which passes 'javaMethodCall' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private JavaMethodCallCodeGenerator() {
		super("javaMethodCall");
	}

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<JavaMethodCallStruct> event) {
		super.onGeneratorEvent(event);
	}

	@Override
	protected void generateSpecialOperator(final JavaMethodCallStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String methodName = input.getMethodName().getJavaName();
		mv.visitLdcInsn(methodName);
		final int methodNameStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, methodNameStore);

		final List<LispStruct> arguments = input.getArguments();
		final int numberOfArguments = arguments.size();

		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);
		final int methodArgumentsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, methodArgumentsStore);

		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.JAVA_CLASS_NAME);
		final int methodArgumentTypesStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, methodArgumentTypesStore);

		if (!arguments.isEmpty()) {
			mv.visitInsn(Opcodes.ICONST_0);
			final int indexStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ISTORE, indexStore);

			final int argumentStore = methodBuilder.getNextAvailableStore();

			for (final LispStruct argument : arguments) {
				codeGenerator.generate(argument, generatorState);
				mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

				mv.visitVarInsn(Opcodes.ALOAD, methodArgumentsStore);
				mv.visitVarInsn(Opcodes.ILOAD, indexStore);
				mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
				mv.visitInsn(Opcodes.AASTORE);

				mv.visitVarInsn(Opcodes.ALOAD, methodArgumentTypesStore);
				mv.visitVarInsn(Opcodes.ILOAD, indexStore);
				mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   GenerationConstants.JAVA_OBJECT_NAME,
				                   GenerationConstants.JAVA_OBJECT_GET_CLASS_METHOD_NAME,
				                   GenerationConstants.JAVA_OBJECT_GET_CLASS_METHOD_DESC,
				                   false);
				mv.visitInsn(Opcodes.AASTORE);

				mv.visitIincInsn(indexStore, 1);
			}
		}

		final LispStruct javaObject = input.getJavaObject();
		codeGenerator.generate(javaObject, generatorState);
		final int javaObjectStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, javaObjectStore);

		mv.visitVarInsn(Opcodes.ALOAD, javaObjectStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_OBJECT_NAME,
		                   GenerationConstants.JAVA_OBJECT_GET_CLASS_METHOD_NAME,
		                   GenerationConstants.JAVA_OBJECT_GET_CLASS_METHOD_DESC,
		                   false);

		mv.visitVarInsn(Opcodes.ALOAD, methodNameStore);
		mv.visitVarInsn(Opcodes.ALOAD, methodArgumentTypesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_CLASS_NAME,
		                   GenerationConstants.JAVA_CLASS_GET_DECLARED_METHOD_METHOD_NAME,
		                   GenerationConstants.JAVA_CLASS_GET_DECLARED_METHOD_METHOD_DESC,
		                   false);
		final int javaMethodStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, javaMethodStore);

		mv.visitVarInsn(Opcodes.ALOAD, javaMethodStore);
		mv.visitVarInsn(Opcodes.ALOAD, javaObjectStore);
		mv.visitVarInsn(Opcodes.ALOAD, methodArgumentsStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_METHOD_NAME,
		                   GenerationConstants.JAVA_METHOD_INVOKE_METHOD_NAME,
		                   GenerationConstants.JAVA_METHOD_INVOKE_METHOD_DESC,
		                   false);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
