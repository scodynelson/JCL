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
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.lang.ArrayStruct;
import jcl.lang.LispStruct;
import jcl.lang.internal.ArrayStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ArrayStruct} objects dynamically by utilizing the {@link ArrayStruct#getDimensions()} and {@link
 * ArrayStruct#getContents()} of the provided {@link ArrayStruct} input value.
 */
@Component
class ArrayCodeGenerator implements CodeGenerator<ArrayStructImpl> {

	/**
	 * {@link QuoteCodeGenerator} used for generating the {@link ArrayStruct} contents as if they were quoted values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ArrayStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link ArrayStruct#getDimensions()} values</li>
	 * <li>Building the {@link ArrayStruct#getContents()} values, ensuring that each content value is treated as being
	 * 'quoted'</li>
	 * <li>Constructing a new {@link ArrayStruct} with the built dimension and content {@link List}s</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link ArrayStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<ArrayStructImpl> event) {
		final ArrayStruct input = event.getSource();
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
		final int dimensionsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, dimensionsStore);

		final List<Integer> dimensions = input.getDimensions();
		for (final Integer dimension : dimensions) {
			mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
			mv.visitLdcInsn(dimension);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.JAVA_INTEGER_NAME,
			                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_NAME,
			                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_DESC,
			                   false);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_LIST_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}

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

		mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_ARRAY_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_ARRAY_METHOD_DESC,
		                   false);
	}
}
