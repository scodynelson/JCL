/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.symbols.TStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link TStruct} objects dynamically by utilizing the static singleton {@link TStruct#INSTANCE}
 * value.
 */
@Component
final class TCodeGenerator implements CodeGenerator<TStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link TStruct} class.
	 */
	private static final String T_STRUCT_NAME = Type.getInternalName(TStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link TStruct} class.
	 */
	private static final String T_STRUCT_DESC = Type.getDescriptor(TStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link TStruct} objects, by retrieving the static singleton {@link TStruct#INSTANCE}.
	 *
	 * @param input
	 * 		the {@link TStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<TStruct> event) {
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, T_STRUCT_NAME, GenerationConstants.SINGLETON_INSTANCE, T_STRUCT_DESC);
	}
}
