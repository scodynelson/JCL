/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.array.StringStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link StringStructImpl} objects dynamically by utilizing {@link StringStructImpl#getAsJavaString()} to
 * get the equivalent {@link String} value of the provided {@link StringStructImpl} input value.
 */
@Component
final class StringCodeGenerator implements CodeGenerator<StringStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link StringStructImpl} class.
	 */
	private static final String STRING_STRUCT_NAME = Type.getInternalName(StringStructImpl.class);

	/**
	 * Constant {@link String} containing the description for the {@link StringStructImpl#StringStruct(String)}
	 * constructor method.
	 */
	private static final String STRING_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(StringStructImpl.class, String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link StringStructImpl} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link String} constant produced by performing {@link StringStructImpl#getAsJavaString()}</li>
	 * <li>Constructing a new {@link StringStructImpl} with the loaded {@link String} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link StringStructImpl} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<StringStructImpl> event) {
		final StringStructImpl input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, STRING_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		final String javaString = input.getAsJavaString();
		mv.visitLdcInsn(javaString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   STRING_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   STRING_STRUCT_INIT_DESC,
		                   false);
	}
}
