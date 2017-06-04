/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.ComplexStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.lang.internal.ComplexStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ComplexStruct} objects dynamically by utilizing the {@link NumberStruct#realPart()} and
 * {@link NumberStruct#imagPart()} of the provided {@link ComplexStruct} input value.
 */
@Component
final class ComplexCodeGenerator implements CodeGenerator<ComplexStructImpl> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ComplexStruct} real and imaginary values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Constant {@link String} containing the name for the {@link ComplexStruct} class.
	 */
	private static final String COMPLEX_NAME = Type.getInternalName(ComplexStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link ComplexStruct#toLispComplex(RealStruct, RealStruct)}
	 * method.
	 */
	private static final String COMPLEX_TO_LISP_COMPLEX_METHOD_NAME = "toLispComplex";

	/**
	 * Constant {@link String} containing the description for the {@link ComplexStruct#toLispComplex(RealStruct,
	 * RealStruct)} method.
	 */
	private static final String COMPLEX_TO_LISP_COMPLEX_METHOD_DESC
			= CodeGenerators.getMethodDescription(ComplexStruct.class, COMPLEX_TO_LISP_COMPLEX_METHOD_NAME,
			                                      RealStruct.class, RealStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ComplexStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link NumberStruct#realPart()} value.</li>
	 * <li>Emitting the {@link NumberStruct#imagPart()} value.</li>
	 * <li>Retrieving a {@link ComplexStruct} via {@link ComplexStruct#toLispComplex(RealStruct, RealStruct)} with
	 * the created real and imaginary values</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link ComplexStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<ComplexStructImpl> event) {
		final ComplexStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		codeGenerator.generate(input.realPart(), generatorState);
		codeGenerator.generate(input.imagPart(), generatorState);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   COMPLEX_NAME,
		                   COMPLEX_TO_LISP_COMPLEX_METHOD_NAME,
		                   COMPLEX_TO_LISP_COMPLEX_METHOD_DESC,
		                   true);
	}
}
