/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.array.StringStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link StringStruct} objects dynamically by utilizing {@link StringStruct#getAsJavaString()} to
 * get the equivalent {@link String} value of the provided {@link StringStruct} input value.
 */
@Component
final class StringCodeGenerator implements CodeGenerator<StringStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link StringStruct} class.
	 */
	private static final String STRING_STRUCT_NAME = Type.getInternalName(StringStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link StringStruct#StringStruct(String)}
	 * constructor method.
	 */
	private static final String STRING_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(StringStruct.class, String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link StringStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link String} constant produced by performing {@link StringStruct#getAsJavaString()}</li>
	 * <li>Constructing a new {@link StringStruct} with the loaded {@link String} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link StringStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<StringStruct> event) {
		final StringStruct input = event.getSource();
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