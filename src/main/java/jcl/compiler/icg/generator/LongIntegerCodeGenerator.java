/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.LongIntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link LongIntegerStruct} objects dynamically by utilizing the {@link LongIntegerStruct#l} of the
 * provided {@link LongIntegerStruct} input value.
 */
@Component
class LongIntegerCodeGenerator implements CodeGenerator<LongIntegerStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link LongIntegerStruct} class.
	 */
	private static final String LONG_INTEGER_STRUCT_NAME = Type.getInternalName(LongIntegerStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link LongIntegerStruct#valueOf(long)} method.
	 */
	private static final String LONG_INTEGER_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link LongIntegerStruct#valueOf(long)} method.
	 */
	private static final String LONG_INTEGER_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(LongIntegerStruct.class, LONG_INTEGER_STRUCT_VALUE_OF_METHOD_NAME, long.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LongIntegerStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving a {@link LongIntegerStruct} via {@link LongIntegerStruct#valueOf(long)} with the {@link
	 * LongIntegerStruct#l} {@code long} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link LongIntegerStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final LongIntegerStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final long l = input.longValue();
		mv.visitLdcInsn(l);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   LONG_INTEGER_STRUCT_NAME,
		                   LONG_INTEGER_STRUCT_VALUE_OF_METHOD_NAME,
		                   LONG_INTEGER_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
