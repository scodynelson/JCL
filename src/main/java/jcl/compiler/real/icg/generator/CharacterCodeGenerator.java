/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
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
	 * Constant {@link String} containing the description for the {@link CharacterStruct#CharacterStruct(int)}
	 * constructor method.
	 */
	private static final String CHARACTER_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(CharacterStruct.class, int.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link CharacterStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link CharacterStruct#codePoint} constant</li>
	 * <li>Constructing a new {@link CharacterStruct} with the loaded code point value</li>
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

		mv.visitTypeInsn(Opcodes.NEW, CHARACTER_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		final int codePoint = input.getCodePoint();
		mv.visitLdcInsn(codePoint);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				CHARACTER_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				CHARACTER_STRUCT_INIT_DESC,
				false);
	}
}
