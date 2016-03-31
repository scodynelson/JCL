/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigInteger;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.BigIntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link BigIntegerStruct} objects dynamically by utilizing the {@link BigIntegerStruct#bigInteger}
 * of the provided {@link BigIntegerStruct} input value.
 */
@Component
class BigIntegerCodeGenerator implements CodeGenerator<BigIntegerStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link BigIntegerStruct} class.
	 */
	private static final String BIG_INTEGER_STRUCT_NAME = Type.getInternalName(BigIntegerStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link BigIntegerStruct#valueOf(BigInteger)} method.
	 */
	private static final String BIG_INTEGER_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link BigIntegerStruct#valueOf(BigInteger)} method.
	 */
	private static final String BIG_INTEGER_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(BigIntegerStruct.class, BIG_INTEGER_STRUCT_VALUE_OF_METHOD_NAME, BigInteger.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BigIntegerStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link BigIntegerStruct#bigInteger} value</li>
	 * <li>Retrieving a {@link BigIntegerStruct} via {@link BigIntegerStruct#valueOf(BigInteger)} with the {@link
	 * BigIntegerStruct#bigInteger} {@link BigInteger} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link BigIntegerStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final BigIntegerStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger bigInteger = input.bigIntegerValue();
		final String integerString = bigInteger.toString();
		mv.visitLdcInsn(integerString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_BIG_INTEGER_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_BIG_INTEGER_INIT_DESC,
		                   false);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   BIG_INTEGER_STRUCT_NAME,
		                   BIG_INTEGER_STRUCT_VALUE_OF_METHOD_NAME,
		                   BIG_INTEGER_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
