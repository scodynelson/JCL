/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.numbers.ComplexStruct;
import jcl.numbers.RealStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ComplexStruct} objects dynamically by utilizing the {@link ComplexStruct#real} and {@link
 * ComplexStruct#imaginary} of the provided {@link ComplexStruct} input value.
 */
@Component
class ComplexCodeGenerator implements CodeGenerator<ComplexStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link ComplexStruct} class.
	 */
	private static final String COMPLEX_STRUCT_NAME = Type.getInternalName(ComplexStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link ComplexStruct#ComplexStruct(RealStruct,
	 * RealStruct)} constructor method.
	 */
	private static final String COMPLEX_STRUCT_INIT_DESC
			= GeneratorUtils.getConstructorDescription(ComplexStruct.class, RealStruct.class, RealStruct.class);

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ComplexStruct} real and imaginary values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ComplexStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link ComplexStruct#real} value</li>
	 * <li>Building the {@link ComplexStruct#imaginary} value</li>
	 * <li>Constructing a new {@link ComplexStruct} with the built real and imaginary values</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link ComplexStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final ComplexStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final RealStruct real = input.getReal();
		codeGenerator.generate(real, generatorState);
		final int realStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, realStore);

		final RealStruct imaginary = input.getImaginary();
		codeGenerator.generate(imaginary, generatorState);
		final int imaginaryStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, imaginaryStore);

		mv.visitTypeInsn(Opcodes.NEW, COMPLEX_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, realStore);
		mv.visitVarInsn(Opcodes.ALOAD, imaginaryStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				COMPLEX_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				COMPLEX_STRUCT_INIT_DESC,
				false);
	}
}
