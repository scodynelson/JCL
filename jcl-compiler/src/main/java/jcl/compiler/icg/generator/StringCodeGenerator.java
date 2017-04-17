/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.StringStruct;
import jcl.lang.internal.SimpleStringStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link StringStruct} objects dynamically by utilizing {@link StringStruct#toJavaString()} to
 * get the equivalent {@link String} value of the provided {@link StringStruct} input value.
 */
@Component
final class StringCodeGenerator implements CodeGenerator<SimpleStringStructImpl> {

	/**
	 * {@inheritDoc}
	 * Generation method for {@link StringStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link String} constant produced by performing {@link StringStruct#toJavaString()}</li>
	 * <li>Constructing a new {@link StringStruct} with the loaded {@link String} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link StringStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<SimpleStringStructImpl> event) {
		final StringStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String javaString = input.toJavaString();
		mv.visitLdcInsn(javaString);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_STRING_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_STRING_METHOD_DESC,
		                   false);
	}
}
