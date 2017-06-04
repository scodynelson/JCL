/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigInteger;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.BignumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.internal.BignumStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link BignumStruct} objects dynamically by utilizing the {@link IntegerStruct#toJavaBigInteger()}
 * of the provided {@link BignumStruct} input value.
 */
@Component
final class BignumCodeGenerator implements CodeGenerator<BignumStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct} class.
	 */
	private static final String INTEGER_NAME = Type.getInternalName(IntegerStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct#toLispInteger(BigInteger)} method.
	 */
	private static final String INTEGER_TO_LISP_INTEGER_METHOD_NAME = "toLispInteger";

	/**
	 * Constant {@link String} containing the description for the {@link IntegerStruct#toLispInteger(BigInteger)}
	 * method.
	 */
	private static final String INTEGER_TO_LISP_INTEGER_METHOD_DESC
			= CodeGenerators.getMethodDescription(IntegerStruct.class, INTEGER_TO_LISP_INTEGER_METHOD_NAME,
			                                      BigInteger.class);

	private static final String JAVA_BIG_INTEGER_NAME = Type.getInternalName(BigInteger.class);

	private static final String JAVA_BIG_INTEGER_INIT_DESC = CodeGenerators.getConstructorDescription(BigInteger.class,
	                                                                                                  String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BignumStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link IntegerStruct#toJavaBigInteger()} value.</li>
	 * <li>Retrieving a {@link BignumStruct} via {@link IntegerStruct#toLispInteger(BigInteger)} with the emitted
	 * {@link BigInteger} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link BignumStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<BignumStructImpl> event) {
		final BignumStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, JAVA_BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger bigInteger = input.toJavaBigInteger();
		mv.visitLdcInsn(bigInteger.toString());

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   JAVA_BIG_INTEGER_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   JAVA_BIG_INTEGER_INIT_DESC,
		                   false);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   INTEGER_NAME,
		                   INTEGER_TO_LISP_INTEGER_METHOD_NAME,
		                   INTEGER_TO_LISP_INTEGER_METHOD_DESC,
		                   true);
	}
}
