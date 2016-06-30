/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link VectorStruct} objects dynamically by utilizing the {@link VectorStruct#contents} of
 * the provided {@link VectorStruct} input value.
 */
@Component
final class VectorCodeGenerator implements CodeGenerator<VectorStruct<LispStruct>> {

	/**
	 * Constant {@link String} containing the name for the {@link VectorStruct} class.
	 */
	private static final String VECTOR_STRUCT_NAME = Type.getInternalName(VectorStruct.class);

	/**
	 * Constant {@link String} containing the description for the {@link VectorStruct#VectorStruct(List)}
	 * constructor method.
	 */
	private static final String VECTOR_STRUCT_INIT_DESC
			= CodeGenerators.getConstructorDescription(VectorStruct.class, List.class);

	/**
	 * {@link QuoteCodeGenerator} used for generating the {@link VectorStruct} contents as if they were quoted values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link VectorStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link VectorStruct#contents}, ensuring that each content value is treated as being
	 * 'quoted'</li>
	 * <li>Constructing a new {@link VectorStruct} with the built content {@link List}</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link VectorStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<VectorStruct<LispStruct>> event) {
		final VectorStruct<LispStruct> input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_ARRAY_LIST_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
		                   false);
		final int contentsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		final int contentStore = methodBuilder.getNextAvailableStore();

		final List<LispStruct> contents = input.getContents();
		for (final LispStruct content : contents) {
			final QuoteStruct quotedContent = new QuoteStruct(content);
			codeGenerator.generate(quotedContent, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, contentStore);

			mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
			mv.visitVarInsn(Opcodes.ALOAD, contentStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_LIST_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitTypeInsn(Opcodes.NEW, VECTOR_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   VECTOR_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   VECTOR_STRUCT_INIT_DESC,
		                   false);
	}
}
