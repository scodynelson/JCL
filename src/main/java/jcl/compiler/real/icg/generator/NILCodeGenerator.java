/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.symbols.NILStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link NILStruct} objects dynamically by utilizing the static singleton {@link NILStruct#INSTANCE}
 * value.
 */
@Component
class NILCodeGenerator implements CodeGenerator<NILStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link NILStruct} class.
	 */
	private static final String NIL_STRUCT_NAME = Type.getInternalName(NILStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link NILStruct} class.
	 */
	private static final String NIL_STRUCT_DESC = Type.getDescriptor(NILStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link NILStruct} objects, by retrieving the static singleton {@link NILStruct#INSTANCE}.
	 *
	 * @param input
	 * 		the {@link NILStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final NILStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, NIL_STRUCT_NAME, GenerationConstants.SINGLETON_INSTANCE, NIL_STRUCT_DESC);
	}
}
