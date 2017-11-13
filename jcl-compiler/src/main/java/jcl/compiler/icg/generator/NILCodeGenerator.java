/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.NILStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link NILStruct} objects dynamically by utilizing the static singleton {@link NILStruct#INSTANCE}
 * value.
 */
@Component
final class NILCodeGenerator implements CodeGenerator<NILStruct> {

	/**
	 * {@inheritDoc}
	 * Generation method for {@link NILStruct} objects, by retrieving the static singleton {@link NILStruct#INSTANCE}.
	 *
	 * @param input
	 * 		the {@link NILStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<NILStruct> event) {
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  GenerationConstants.NIL_STRUCT_NAME,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  GenerationConstants.NIL_STRUCT_DESC);
	}
}
