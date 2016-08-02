/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.IntegerStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import org.apfloat.Apint;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link IntegerStruct} objects dynamically by utilizing the {@link IntegerStruct#ap()} of the provided
 * {@link IntegerStruct} input value.
 */
@Component
final class IntegerCodeGenerator implements CodeGenerator<IntegerStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link Apint} class.
	 */
	private static final String APINT_NAME = Type.getInternalName(Apint.class);

	/**
	 * Constant {@link String} containing the description for the {@link Apint#Apint(String)} constructor method.
	 */
	private static final String APINT_INIT_METHOD_DESC
			= CodeGenerators.getConstructorDescription(Apint.class, String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link IntegerStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Constructing a new {@link Apint} from the {@link String} representation of the {@link IntegerStruct#ap()}
	 * value</li>
	 * <li>Retrieving a {@link IntegerStruct} via {@link LispStructFactory#toInteger(Apint)} with the created {@link Apint}
	 * value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link IntegerStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<IntegerStructImpl> event) {
		final IntegerStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, APINT_NAME);
		mv.visitInsn(Opcodes.DUP);

		final String apString = input.ap().toString();
		mv.visitLdcInsn(apString);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   APINT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   APINT_INIT_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_INTEGER_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_INTEGER_METHOD_DESC,
		                   false);
	}
}
