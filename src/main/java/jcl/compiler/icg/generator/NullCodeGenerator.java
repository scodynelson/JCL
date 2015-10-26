/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lists.NullStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link NullStruct} objects dynamically by utilizing the static singleton {@link
 * NullStruct#INSTANCE} value.
 */
@Component
class NullCodeGenerator implements CodeGenerator<NullStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link NullStruct} class.
	 */
	private static final String NULL_STRUCT_NAME = Type.getInternalName(NullStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link NullStruct} class.
	 */
	private static final String NULL_STRUCT_DESC = Type.getDescriptor(NullStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link NullStruct} objects, by retrieving the static singleton {@link
	 * NullStruct#INSTANCE}.
	 *
	 * @param input
	 * 		the {@link NullStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final NullStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, NULL_STRUCT_NAME, GenerationConstants.SINGLETON_INSTANCE, NULL_STRUCT_DESC);
	}
}
