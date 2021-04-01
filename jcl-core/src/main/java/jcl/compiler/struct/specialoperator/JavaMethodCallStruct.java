/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.java.JavaNameStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class JavaMethodCallStruct extends CompilerSpecialOperatorStruct {

	private final JavaNameStruct methodName;
	private final LispStruct javaObject;
	private final List<LispStruct> arguments;

	public JavaMethodCallStruct(final JavaNameStruct methodName, final LispStruct javaObject,
	                            final List<LispStruct> arguments) {
		super("javaMethodCall");
		this.methodName = methodName;
		this.javaObject = javaObject;
		this.arguments = arguments;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(JAVA-METHOD-CALL ");

		builder.append(methodName);
		builder.append(' ');
		builder.append(javaObject);
		builder.append(' ');

		final String argumentsString
				= arguments.stream()
				           .map(Object::toString)
				           .collect(Collectors.joining(" "));
		builder.append(argumentsString);
		builder.append(')');

		return builder.toString();
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String javaMethodName = methodName.getJavaName();
		mv.visitLdcInsn(javaMethodName);
		final int methodNameStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, methodNameStore);

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
				argument.generate(generatorState);
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

		javaObject.generate(generatorState);
		final int javaObjectStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, javaObjectStore);

		mv.visitVarInsn(Opcodes.ALOAD, methodNameStore);
		mv.visitVarInsn(Opcodes.ALOAD, javaObjectStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_OBJECT_NAME,
		                   GenerationConstants.JAVA_OBJECT_GET_CLASS_METHOD_NAME,
		                   GenerationConstants.JAVA_OBJECT_GET_CLASS_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ALOAD, methodArgumentTypesStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.JAVA_METHOD_STRUCT_NAME,
		                   GenerationConstants.JAVA_METHOD_STRUCT_TO_JAVA_REFLECTION_METHOD_METHOD_NAME,
		                   GenerationConstants.JAVA_METHOD_STRUCT_TO_JAVA_REFLECTION_METHOD_METHOD_DESC,
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
