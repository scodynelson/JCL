/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.characters.CharacterStruct;
import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link CharacterStruct} objects dynamically by utilizing the {@link CharacterStruct#codePoint} of
 * the provided {@link CharacterStruct} input value.
 */
@Component
class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link CharacterStruct} class.
	 */
	private static final String CHARACTER_STRUCT_NAME = Type.getInternalName(CharacterStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link CharacterStruct#valueOf(Integer)} method.
	 */
	private static final String CHARACTER_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link CharacterStruct#valueOf(Integer)} method.
	 */
	private static final String CHARACTER_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(CharacterStruct.class, CHARACTER_STRUCT_VALUE_OF_METHOD_NAME, Integer.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link CharacterStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link CharacterStruct#codePoint} constant</li>
	 * <li>Retrieving a {@link CharacterStruct} with the loaded code point value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link CharacterStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final CharacterStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int codePoint = input.getCodePoint();
		mv.visitLdcInsn(codePoint);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.JAVA_INTEGER_NAME,
		                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_NAME,
		                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   CHARACTER_STRUCT_NAME,
		                   CHARACTER_STRUCT_VALUE_OF_METHOD_NAME,
		                   CHARACTER_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
