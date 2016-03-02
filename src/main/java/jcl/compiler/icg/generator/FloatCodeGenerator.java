/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.math.BigDecimal;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.FloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link FloatStruct} objects dynamically by utilizing the {@link FloatStruct#bigDecimal} of
 * the provided {@link FloatStruct} input value.
 */
@Component
class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link FloatStruct} class.
	 */
	private static final String FLOAT_STRUCT_NAME = Type.getInternalName(FloatStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link FloatStruct#FloatStruct(BigDecimal)}
	 * constructor method.
	 */
	private static final String FLOAT_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(FloatStruct.class, BigDecimal.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link FloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link FloatStruct#bigDecimal} value</li>
	 * <li>Constructing a new {@link FloatStruct} with the built {@link BigDecimal} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link FloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final FloatStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_BIG_DECIMAL_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigDecimal bigDecimal = input.getBigDecimal();
		final String decimalString = bigDecimal.toString();
		mv.visitLdcInsn(decimalString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_BIG_DECIMAL_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_BIG_DECIMAL_INIT_DESC,
		                   false);
		final int bigDecimalStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, bigDecimalStore);

		mv.visitTypeInsn(Opcodes.NEW, FLOAT_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, bigDecimalStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   FLOAT_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   FLOAT_STRUCT_INIT_DESC,
		                   false);
	}
}
