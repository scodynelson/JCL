/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.number.IntegerStructImpl;
import org.apfloat.Apint;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link IntegerStructImpl} objects dynamically by utilizing the {@link IntegerStructImpl#ap} of the provided
 * {@link IntegerStructImpl} input value.
 */
@Component
final class IntegerCodeGenerator implements CodeGenerator<IntegerStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStructImpl} class.
	 */
	private static final String INTEGER_STRUCT_NAME = Type.getInternalName(IntegerStructImpl.class);

	/**
	 * Constant {@link String} containing the name for the {@link IntegerStructImpl#valueOf(Apint)} method.
	 */
	private static final String INTEGER_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link IntegerStructImpl#valueOf(Apint)} method.
	 */
	private static final String INTEGER_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(IntegerStructImpl.class, INTEGER_STRUCT_VALUE_OF_METHOD_NAME, Apint.class);

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
	 * Generation method for {@link IntegerStructImpl} objects, by performing the following operations:
	 * <ol>
	 * <li>Constructing a new {@link Apint} from the {@link String} representation of the {@link IntegerStructImpl#ap}
	 * value</li>
	 * <li>Retrieving a {@link IntegerStructImpl} via {@link IntegerStructImpl#valueOf(Apint)} with the created {@link Apint}
	 * value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link IntegerStructImpl} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<IntegerStructImpl> event) {
		final IntegerStructImpl input = event.getSource();
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
		                   INTEGER_STRUCT_NAME,
		                   INTEGER_STRUCT_VALUE_OF_METHOD_NAME,
		                   INTEGER_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
