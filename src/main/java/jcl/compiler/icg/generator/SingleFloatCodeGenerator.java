/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.SingleFloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link SingleFloatStruct} objects dynamically by utilizing the {@link SingleFloatStruct#f} of the
 * provided {@link SingleFloatStruct} input value.
 */
@Component
class SingleFloatCodeGenerator implements CodeGenerator<SingleFloatStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link SingleFloatStruct} class.
	 */
	private static final String SINGLE_FLOAT_STRUCT_NAME = Type.getInternalName(SingleFloatStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link SingleFloatStruct#valueOf(float)} method.
	 */
	private static final String SINGLE_FLOAT_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link SingleFloatStruct#valueOf(float)} method.
	 */
	private static final String SINGLE_FLOAT_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(SingleFloatStruct.class, SINGLE_FLOAT_STRUCT_VALUE_OF_METHOD_NAME, float.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link SingleFloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving a {@link SingleFloatStruct} via {@link SingleFloatStruct#valueOf(float)} with the {@link
	 * SingleFloatStruct#f} {@code float} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link SingleFloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final SingleFloatStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final float f = input.floatValue();
		mv.visitLdcInsn(f);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   SINGLE_FLOAT_STRUCT_NAME,
		                   SINGLE_FLOAT_STRUCT_VALUE_OF_METHOD_NAME,
		                   SINGLE_FLOAT_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
