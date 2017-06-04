/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.DoubleFloatStruct;
import jcl.lang.internal.DoubleFloatStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link DoubleFloatStruct} objects dynamically by utilizing the {@link
 * DoubleFloatStruct#toJavaPDouble()} of the provided {@link DoubleFloatStruct} input value.
 */
@Component
final class DoubleFloatCodeGenerator implements CodeGenerator<DoubleFloatStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link DoubleFloatStruct} class.
	 */
	private static final String DOUBLE_FLOAT_NAME = Type.getInternalName(DoubleFloatStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link DoubleFloatStruct#toLispFloat(double)} method.
	 */
	private static final String DOUBLE_FLOAT_TO_LISP_FLOAT_METHOD_NAME = "toLispFloat";

	/**
	 * Constant {@link String} containing the description for the {@link DoubleFloatStruct#toLispFloat(double)} method.
	 */
	private static final String DOUBLE_FLOAT_TO_LISP_FLOAT_METHOD_DESC
			= CodeGenerators.getMethodDescription(DoubleFloatStruct.class, DOUBLE_FLOAT_TO_LISP_FLOAT_METHOD_NAME, double.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link DoubleFloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link DoubleFloatStruct#toJavaPDouble()} value.</li>
	 * <li>Retrieving a {@link DoubleFloatStruct} via {@link DoubleFloatStruct#toLispFloat(double)} with the emitted
	 * {@code float} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link DoubleFloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<DoubleFloatStructImpl> event) {
		final DoubleFloatStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final double d = input.toJavaPDouble();
		mv.visitLdcInsn(d);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   DOUBLE_FLOAT_NAME,
		                   DOUBLE_FLOAT_TO_LISP_FLOAT_METHOD_NAME,
		                   DOUBLE_FLOAT_TO_LISP_FLOAT_METHOD_DESC,
		                   true);
	}
}
