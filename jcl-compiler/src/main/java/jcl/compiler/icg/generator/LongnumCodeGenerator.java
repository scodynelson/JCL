/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.IntegerStruct;
import jcl.lang.LongnumStruct;
import jcl.lang.internal.LongnumStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link LongnumStruct} objects dynamically by utilizing the {@link IntegerStruct#toJavaLong()} of
 * the provided {@link LongnumStruct} input value.
 */
@Component
final class LongnumCodeGenerator implements CodeGenerator<LongnumStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct} class.
	 */
	private static final String INTEGER_NAME = Type.getInternalName(IntegerStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct#toLispInteger(long)} method.
	 */
	private static final String INTEGER_TO_LISP_INTEGER_METHOD_NAME = "toLispInteger";

	/**
	 * Constant {@link String} containing the description for the {@link IntegerStruct#toLispInteger(long)} method.
	 */
	private static final String INTEGER_TO_LISP_INTEGER_METHOD_DESC
			= CodeGenerators.getMethodDescription(IntegerStruct.class, INTEGER_TO_LISP_INTEGER_METHOD_NAME, long.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LongnumStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link IntegerStruct#toJavaPLong()} value.</li>
	 * <li>Retrieving a {@link LongnumStruct} via {@link IntegerStruct#toLispInteger(long)} with the emitted
	 * {@code long} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link LongnumStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<LongnumStructImpl> event) {
		final LongnumStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final long l = input.toJavaPLong();
		mv.visitLdcInsn(l);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   INTEGER_NAME,
		                   INTEGER_TO_LISP_INTEGER_METHOD_NAME,
		                   INTEGER_TO_LISP_INTEGER_METHOD_DESC,
		                   true);
	}
}
