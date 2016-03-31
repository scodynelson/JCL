/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigDecimal;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.BigFloatStruct;
import jcl.numbers.FloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link BigFloatStruct} objects dynamically by utilizing the {@link BigFloatStruct#bigDecimal} of
 * the provided {@link FloatStruct} input value.
 */
@Component
class BigFloatCodeGenerator implements CodeGenerator<BigFloatStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link BigFloatStruct} class.
	 */
	private static final String BIG_FLOAT_STRUCT_NAME = Type.getInternalName(BigFloatStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link BigFloatStruct#valueOf(BigDecimal)} method.
	 */
	private static final String BIG_FLOAT_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link BigFloatStruct#valueOf(BigDecimal)} method.
	 */
	private static final String BIG_FLOAT_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(BigFloatStruct.class, BIG_FLOAT_STRUCT_VALUE_OF_METHOD_NAME, BigDecimal.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BigFloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link BigFloatStruct#bigDecimal} value</li>
	 * <li>Retrieving a {@link BigFloatStruct} via {@link BigFloatStruct#valueOf(BigDecimal)} with the {@link
	 * BigFloatStruct#bigDecimal} {@link BigDecimal} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link BigFloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final BigFloatStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_BIG_DECIMAL_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigDecimal bigDecimal = input.bigDecimalValue();
		final String decimalString = bigDecimal.toString();
		mv.visitLdcInsn(decimalString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_BIG_DECIMAL_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_BIG_DECIMAL_INIT_DESC,
		                   false);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   BIG_FLOAT_STRUCT_NAME,
		                   BIG_FLOAT_STRUCT_VALUE_OF_METHOD_NAME,
		                   BIG_FLOAT_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
