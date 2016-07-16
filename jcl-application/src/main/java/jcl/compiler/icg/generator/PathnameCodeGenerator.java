/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.net.URI;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.pathname.PathnameStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link PathnameStruct} objects dynamically by utilizing the {@link PathnameStruct#getUri()} of
 * the provided {@link PathnameStruct} input value.
 */
@Component
final class PathnameCodeGenerator implements CodeGenerator<PathnameStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link PathnameStruct} class.
	 */
	private static final String PATHNAME_STRUCT_NAME = Type.getInternalName(PathnameStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link PathnameStruct#PathnameStruct(URI)}
	 * constructor method.
	 */
	private static final String PATHNAME_STRUCT_NAME_INIT_DESC
			= CodeGenerators.getConstructorDescription(PathnameStruct.class, URI.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link PathnameStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link PathnameStruct#uri} value</li>
	 * <li>Constructing a new {@link PathnameStruct} with the built {@link URI} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link PathnameStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<PathnameStruct> event) {
		final PathnameStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final URI uri = input.getUri();
		final String filePath = uri.toString();
		mv.visitLdcInsn(filePath);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.JAVA_URI_NAME,
		                   GenerationConstants.JAVA_URI_CREATE_METHOD_NAME,
		                   GenerationConstants.JAVA_URI_CREATE_METHOD_DESC,
		                   false);
		final int uriStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, uriStore);

		mv.visitTypeInsn(Opcodes.NEW, PATHNAME_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, uriStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   PATHNAME_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   PATHNAME_STRUCT_NAME_INIT_DESC,
		                   false);
	}
}
