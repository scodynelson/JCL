/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigInteger;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.RatioStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link RatioStruct} objects dynamically by utilizing the {@link RatioStruct#bigFraction} of the
 * provided {@link RatioStruct} input value.
 */
@Component
class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link RatioStruct} class.
	 */
	private static final String RATIO_STRUCT_NAME = Type.getInternalName(RatioStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link RatioStruct#RatioStruct(BigInteger,
	 * BigInteger)} constructor method.
	 */
	private static final String RATIO_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(RatioStruct.class, BigInteger.class, BigInteger.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link RatioStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link RatioStruct} numerator value</li>
	 * <li>Building the {@link RatioStruct} denominator value</li>
	 * <li>Constructing a new {@link RatioStruct} with the built numerator and denominator values</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link RatioStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final RatioStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final BigFraction bigFraction = input.getBigFraction();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger numerator = bigFraction.getNumerator();
		final String numeratorString = numerator.toString();
		mv.visitLdcInsn(numeratorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_BIG_INTEGER_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_BIG_INTEGER_INIT_DESC,
		                   false);
		final int numeratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, numeratorStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger denominator = bigFraction.getDenominator();
		final String denominatorString = denominator.toString();
		mv.visitLdcInsn(denominatorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_BIG_INTEGER_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_BIG_INTEGER_INIT_DESC,
		                   false);
		final int denominatorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, denominatorStore);

		mv.visitTypeInsn(Opcodes.NEW, RATIO_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, numeratorStore);
		mv.visitVarInsn(Opcodes.ALOAD, denominatorStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   RATIO_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   RATIO_STRUCT_INIT_DESC,
		                   false);
	}
}
