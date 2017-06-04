/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.IntegerStruct;
import jcl.lang.RatioStruct;
import jcl.lang.RationalStruct;
import jcl.lang.internal.RatioStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link RatioStruct} objects dynamically by utilizing the {@link RationalStruct#numerator()} and
 * {@link RationalStruct#denominator()} of the provided {@link RatioStruct} input value.
 */
@Component
final class RatioCodeGenerator implements CodeGenerator<RatioStructImpl> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link RatioStruct} numerator and denominator values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Constant {@link String} containing the name for the {@link RationalStruct} class.
	 */
	private static final String RATIONAL_NAME = Type.getInternalName(RationalStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link RationalStruct#toLispRational(IntegerStruct,
	 * IntegerStruct)} method.
	 */
	private static final String RATIONAL_TO_LISP_RATIONAL_METHOD_NAME = "toLispRational";

	/**
	 * Constant {@link String} containing the description for the {@link RationalStruct#toLispRational(IntegerStruct,
	 * IntegerStruct)} method.
	 */
	private static final String RATIONAL_TO_LISP_RATIONAL_METHOD_DESC
			= CodeGenerators.getMethodDescription(RationalStruct.class, RATIONAL_TO_LISP_RATIONAL_METHOD_NAME,
			                                      IntegerStruct.class, IntegerStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link RatioStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link RationalStruct#numerator()} value.</li>
	 * <li>Emitting the {@link RationalStruct#denominator()} value.</li>
	 * <li>Retrieving a {@link RatioStruct} via {@link RationalStruct#toLispRational(IntegerStruct, IntegerStruct)} with
	 * the created numerator and denominator values</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link RatioStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<RatioStructImpl> event) {
		final RatioStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		codeGenerator.generate(input.numerator(), generatorState);
		codeGenerator.generate(input.denominator(), generatorState);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   RATIONAL_NAME,
		                   RATIONAL_TO_LISP_RATIONAL_METHOD_NAME,
		                   RATIONAL_TO_LISP_RATIONAL_METHOD_DESC,
		                   true);
	}
}
