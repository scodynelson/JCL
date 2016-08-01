/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.List;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.BitVectorStruct;
import jcl.lang.internal.BitVectorStructImpl;
import jcl.lang.number.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link BitVectorStruct} objects dynamically by utilizing the {@link BitVectorStruct#getContents()} of
 * the provided {@link BitVectorStruct} input value.
 */
@Component
final class BitVectorCodeGenerator implements CodeGenerator<BitVectorStructImpl> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link BitVectorStruct} {@link IntegerStruct} content
	 * values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BitVectorStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link BitVectorStruct#getContents()}, ensuring that each content {@link IntegerStruct} value is
	 * generated properly</li>
	 * <li>Constructing a new {@link BitVectorStruct} with the built content {@link List}</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link BitVectorStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<BitVectorStructImpl> event) {
		final BitVectorStruct input = event.getSource();
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

		final List<IntegerStruct> contents = input.getContents();
		for (final IntegerStruct content : contents) {
			codeGenerator.generate(content, generatorState);
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

		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_BIT_VECTOR_LIST_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_BIT_VECTOR_LIST_METHOD_DESC,
		                   false);
	}
}
