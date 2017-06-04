/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.internal.FixnumStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link FixnumStruct} objects dynamically by utilizing the {@link IntegerStruct#toJavaInt()} of the
 * provided {@link FixnumStruct} input value.
 */
@Component
final class FixnumCodeGenerator implements CodeGenerator<FixnumStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct} class.
	 */
	private static final String INTEGER_NAME = Type.getInternalName(IntegerStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct#toLispInteger(int)} method.
	 */
	private static final String INTEGER_TO_LISP_INTEGER_METHOD_NAME = "toLispInteger";

	/**
	 * Constant {@link String} containing the description for the {@link IntegerStruct#toLispInteger(int)} method.
	 */
	private static final String INTEGER_TO_LISP_INTEGER_METHOD_DESC
			= CodeGenerators.getMethodDescription(IntegerStruct.class, INTEGER_TO_LISP_INTEGER_METHOD_NAME, int.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link FixnumStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link IntegerStruct#toJavaInt()} value.</li>
	 * <li>Retrieving a {@link FixnumStruct} via {@link IntegerStruct#toLispInteger(int)} with the emitted
	 * {@code int} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link FixnumStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<FixnumStructImpl> event) {
		final FixnumStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int i = input.toJavaInt();
		mv.visitLdcInsn(i);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   INTEGER_NAME,
		                   INTEGER_TO_LISP_INTEGER_METHOD_NAME,
		                   INTEGER_TO_LISP_INTEGER_METHOD_DESC,
		                   true);
	}
}
