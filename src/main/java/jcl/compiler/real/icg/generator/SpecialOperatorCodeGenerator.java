/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

abstract class SpecialOperatorCodeGenerator<T extends CompilerSpecialOperatorStruct> implements CodeGenerator<T> {

	private static final String SPECIAL_OPERATOR_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	private final String methodNamePrefix;

	protected SpecialOperatorCodeGenerator(final String methodNamePrefix) {
		this.methodNamePrefix = methodNamePrefix;
	}

	@Override
	public void generate(final T input, final GeneratorState generatorState) {

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String methodName = methodNamePrefix + '_' + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, methodName, SPECIAL_OPERATOR_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		generateSpecialOperator(input, generatorState, methodBuilder, closureArgStore);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, methodName, SPECIAL_OPERATOR_METHOD_DESC, false);
	}

	protected abstract void generateSpecialOperator(T input, GeneratorState generatorState, JavaMethodBuilder methodBuilder,
	                                                int closureArgStore);
}
