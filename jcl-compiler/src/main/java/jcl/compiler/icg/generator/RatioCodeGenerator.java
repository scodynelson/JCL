/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.RatioStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.RatioStructImpl;
import org.apfloat.Aprational;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link RatioStruct} objects dynamically by utilizing the {@link RatioStruct#ap()} of the provided
 * {@link RatioStruct} input value.
 */
@Component
final class RatioCodeGenerator implements CodeGenerator<RatioStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link Aprational} class.
	 */
	private static final String APRATIONAL_NAME = Type.getInternalName(Aprational.class);

	/**
	 * Constant {@link String} containing the description for the {@link Aprational#Aprational(String)} constructor
	 * method.
	 */
	private static final String APRATIONAL_INIT_METHOD_DESC
			= CodeGenerators.getConstructorDescription(Aprational.class, String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link RatioStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Constructing a new {@link Aprational} from the {@link String} representation of the {@link RatioStruct#ap()}
	 * value</li>
	 * <li>Retrieving a {@link RatioStruct} via {@link LispStructFactory#toRatio(Aprational)} with the created {@link
	 * Aprational} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link RatioStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<RatioStructImpl> event) {
		final RatioStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, APRATIONAL_NAME);
		mv.visitInsn(Opcodes.DUP);

		final String apString = input.ap().toString();
		mv.visitLdcInsn(apString);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   APRATIONAL_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   APRATIONAL_INIT_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_RATIO_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_RATIO_METHOD_DESC,
		                   false);
	}
}
