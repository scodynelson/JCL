/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.DoubleFloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link DoubleFloatStruct} objects dynamically by utilizing the {@link DoubleFloatStruct#d} of the
 * provided {@link DoubleFloatStruct} input value.
 */
@Component
class DoubleFloatCodeGenerator implements CodeGenerator<DoubleFloatStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link DoubleFloatStruct} class.
	 */
	private static final String DOUBLE_FLOAT_STRUCT_NAME = Type.getInternalName(DoubleFloatStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link DoubleFloatStruct#valueOf(double)} method.
	 */
	private static final String DOUBLE_FLOAT_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link DoubleFloatStruct#valueOf(double)} method.
	 */
	private static final String DOUBLE_FLOAT_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(DoubleFloatStruct.class, DOUBLE_FLOAT_STRUCT_VALUE_OF_METHOD_NAME, double.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link DoubleFloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving a {@link DoubleFloatStruct} via {@link DoubleFloatStruct#valueOf(double)} with the {@link
	 * DoubleFloatStruct#d} {@code double} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link DoubleFloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final DoubleFloatStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final double d = input.doubleValue();
		mv.visitLdcInsn(d);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   DOUBLE_FLOAT_STRUCT_NAME,
		                   DOUBLE_FLOAT_STRUCT_VALUE_OF_METHOD_NAME,
		                   DOUBLE_FLOAT_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
