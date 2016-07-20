/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.list.ConsStruct;
import jcl.lang.LispStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ConsStruct} objects dynamically by utilizing the {@link ConsStruct#car} and {@link
 * ConsStruct#cdr} of the provided {@link ConsStruct} input value.
 */
@Component
final class ConsCodeGenerator implements CodeGenerator<ConsStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ConsStruct} car and cdr values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ConsStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link ConsStruct#car} value</li>
	 * <li>Building the {@link ConsStruct#cdr} value</li>
	 * <li>Constructing a new {@link ConsStruct} with the built car and cdr values</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link ConsStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<ConsStruct> event) {
		final ConsStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct car = input.getCar();
		codeGenerator.generate(car, generatorState);
		final int carStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, carStore);

		final LispStruct cdr = input.getCdr();
		codeGenerator.generate(cdr, generatorState);
		final int cdrStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, cdrStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.CONS_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, carStore);
		mv.visitVarInsn(Opcodes.ALOAD, cdrStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.CONS_STRUCT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.CONS_STRUCT_INIT_CAR_CDR_DESC,
		                   false);
	}
}
