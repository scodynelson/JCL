/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.List;

import jcl.arrays.BitVectorStruct;
import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link BitVectorStruct} objects dynamically by utilizing the {@link BitVectorStruct#contents} of
 * the provided {@link BitVectorStruct} input value.
 */
@Component
class BitVectorCodeGenerator implements CodeGenerator<BitVectorStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link BitVectorStruct} class.
	 */
	private static final String BIT_VECTOR_STRUCT_NAME = Type.getInternalName(BitVectorStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link BitVectorStruct#BitVectorStruct(List)}
	 * constructor method.
	 */
	private static final String BIT_VECTOR_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(BitVectorStruct.class, List.class);

	/**
	 * {@link IntegerCodeGenerator} used for generating the {@link BitVectorStruct} {@link IntegerStruct} content
	 * values.
	 */
	@Autowired
	private IntegerCodeGenerator integerCodeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BitVectorStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link BitVectorStruct#contents}, ensuring that each content {@link IntegerStruct} value is
	 * generated properly</li>
	 * <li>Constructing a new {@link BitVectorStruct} with the built content {@link List}</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link BitVectorStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final BitVectorStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int contentsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		final int contentStore = methodBuilder.getNextAvailableStore();

		final List<IntegerStruct> contents = input.getContents();
		for (final IntegerStruct content : contents) {
			integerCodeGenerator.generate(content, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, contentStore);

			mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
			mv.visitVarInsn(Opcodes.ALOAD, contentStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitTypeInsn(Opcodes.NEW, BIT_VECTOR_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				BIT_VECTOR_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				BIT_VECTOR_STRUCT_INIT_DESC,
				false);
	}
}
