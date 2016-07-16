/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.Deque;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.function.Closure;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Abstract class for performing generation of {@link CompilerSpecialOperatorStruct}s that create a new method to
 * consolidate their logic.
 *
 * @param <I>
 * 		the type of {@link CompilerSpecialOperatorStruct} to generate code for
 */
abstract class SpecialOperatorCodeGenerator<I extends CompilerSpecialOperatorStruct> implements CodeGenerator<I> {

	/**
	 * Constant {@link String} containing the method description to be used for the creation and invocation of the new
	 * method. Its signature is equivalent to {@code LispStruct methodName(Closure var1)}.
	 */
	private static final String SPECIAL_OPERATOR_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/lang/LispStruct;";

	/**
	 * {@link String} to be used as the method name prefix when creating the new method via {@link
	 * ClassWriter#visitMethod(int, String, String, String, String[])}.
	 */
	private final String methodNamePrefix;

	/**
	 * Protected constructor.
	 *
	 * @param methodNamePrefix
	 * 		the {@link String} to be used as the method name prefix when creating the new method
	 */
	protected SpecialOperatorCodeGenerator(final String methodNamePrefix) {
		this.methodNamePrefix = methodNamePrefix;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link CompilerSpecialOperatorStruct}s that create a new method to consolidate their
	 * logic, by performing the following operations:
	 * <ol>
	 * <li>Visiting a new method via {@link ClassWriter#visitMethod(int, String, String, String, String[])} of the
	 * current {@link JavaClassBuilder#classWriter} from the {@link GeneratorState#classBuilderDeque}</li>
	 * <li>Generating the special operation method content via {@link #generateSpecialOperator(CompilerSpecialOperatorStruct,
	 * GeneratorState, JavaMethodBuilder, int)}</li>
	 * <li>Generating the code to end the new method visitation</li>
	 * <li>Generating the code to invoke the newly created method in place in the previous method execution stack</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link I} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void onGeneratorEvent(final GeneratorEvent<I> event) {
		final I input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaClassBuilder currentClass = generatorState.getCurrentClassBuilder();
		final String className = currentClass.getClassName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String methodName = methodNamePrefix + '_' + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE,
		                                        methodName,
		                                        SPECIAL_OPERATOR_METHOD_DESC,
		                                        null,
		                                        null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Deque<JavaMethodBuilder> methodBuilderDeque = generatorState.getMethodBuilderDeque();
		methodBuilderDeque.addFirst(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		generateSpecialOperator(input, generatorState, methodBuilder, closureArgStore);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderDeque.removeFirst();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderDeque.peekFirst();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                           className,
		                           methodName,
		                           SPECIAL_OPERATOR_METHOD_DESC,
		                           false);
	}

	/**
	 * Abstract method to perform generation logic for the provided {@link I} input value.
	 *
	 * @param input
	 * 		the {@link I} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	protected abstract void generateSpecialOperator(I input, GeneratorState generatorState, JavaMethodBuilder methodBuilder,
	                                                int closureArgStore);
}
