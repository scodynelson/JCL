/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.SingleFloatStruct;
import jcl.lang.internal.SingleFloatStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link SingleFloatStruct} objects dynamically by utilizing the {@link
 * SingleFloatStruct#toJavaPFloat()} of the provided {@link SingleFloatStruct} input value.
 */
@Component
final class SingleFloatCodeGenerator implements CodeGenerator<SingleFloatStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link SingleFloatStruct} class.
	 */
	private static final String SINGLE_FLOAT_NAME = Type.getInternalName(SingleFloatStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link SingleFloatStruct#toLispFloat(float)} method.
	 */
	private static final String SINGLE_FLOAT_TO_LISP_FLOAT_METHOD_NAME = "toLispFloat";

	/**
	 * Constant {@link String} containing the description for the {@link SingleFloatStruct#toLispFloat(float)} method.
	 */
	private static final String SINGLE_FLOAT_TO_LISP_FLOAT_METHOD_DESC
			= CodeGenerators.getMethodDescription(SingleFloatStruct.class, SINGLE_FLOAT_TO_LISP_FLOAT_METHOD_NAME, float.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link SingleFloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link SingleFloatStruct#toJavaPFloat()} value.</li>
	 * <li>Retrieving a {@link SingleFloatStruct} via {@link SingleFloatStruct#toLispFloat(float)} with the emitted
	 * {@code float} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link SingleFloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<SingleFloatStructImpl> event) {
		final SingleFloatStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final float f = input.toJavaPFloat();
		mv.visitLdcInsn(f);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   SINGLE_FLOAT_NAME,
		                   SINGLE_FLOAT_TO_LISP_FLOAT_METHOD_NAME,
		                   SINGLE_FLOAT_TO_LISP_FLOAT_METHOD_DESC,
		                   true);
	}
}
