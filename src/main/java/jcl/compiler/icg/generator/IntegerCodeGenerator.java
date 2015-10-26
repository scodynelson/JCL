/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigInteger;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link IntegerStruct} objects dynamically by utilizing the {@link IntegerStruct#bigInteger} of
 * the provided {@link IntegerStruct} input value.
 */
@Component
class IntegerCodeGenerator implements CodeGenerator<IntegerStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStruct} class.
	 */
	private static final String INTEGER_STRUCT_NAME = Type.getInternalName(IntegerStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link IntegerStruct#IntegerStruct(BigInteger)}
	 * constructor method.
	 */
	private static final String INTEGER_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(IntegerStruct.class, BigInteger.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link IntegerStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link IntegerStruct#bigInteger} value</li>
	 * <li>Constructing a new {@link IntegerStruct} with the built {@link BigInteger} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link IntegerStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final IntegerStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger bigInteger = input.getBigInteger();
		final String integerString = bigInteger.toString();
		mv.visitLdcInsn(integerString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_BIG_INTEGER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_BIG_INTEGER_INIT_DESC,
				false);
		final int bigIntegerStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, bigIntegerStore);

		mv.visitTypeInsn(Opcodes.NEW, INTEGER_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, bigIntegerStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				INTEGER_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				INTEGER_STRUCT_INIT_DESC,
				false);
	}
}
