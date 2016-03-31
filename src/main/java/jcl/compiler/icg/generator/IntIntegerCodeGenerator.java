/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.IntIntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link IntIntegerStruct} objects dynamically by utilizing the {@link IntIntegerStruct#i} of the
 * provided {@link IntIntegerStruct} input value.
 */
@Component
class IntIntegerCodeGenerator implements CodeGenerator<IntIntegerStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link IntIntegerStruct} class.
	 */
	private static final String INT_INTEGER_STRUCT_NAME = Type.getInternalName(IntIntegerStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link IntIntegerStruct#valueOf(int)} method.
	 */
	private static final String INT_INTEGER_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link IntIntegerStruct#valueOf(int)} method.
	 */
	private static final String INT_INTEGER_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(IntIntegerStruct.class, INT_INTEGER_STRUCT_VALUE_OF_METHOD_NAME, int.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link IntIntegerStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving a {@link IntIntegerStruct} via {@link IntIntegerStruct#valueOf(int)} with the {@link
	 * IntIntegerStruct#i} {@code int} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link IntIntegerStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final IntIntegerStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int i = input.intValue();
		mv.visitLdcInsn(i);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   INT_INTEGER_STRUCT_NAME,
		                   INT_INTEGER_STRUCT_VALUE_OF_METHOD_NAME,
		                   INT_INTEGER_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
