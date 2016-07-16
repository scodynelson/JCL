/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.number.FloatStruct;
import org.apfloat.Apfloat;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link FloatStruct} objects dynamically by utilizing the {@link FloatStruct#ap} of the provided
 * {@link FloatStruct} input value.
 */
@Component
final class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	/**
	 * Constant {@link String} containing the name for the {@link FloatStruct} class.
	 */
	private static final String FLOAT_STRUCT_NAME = Type.getInternalName(FloatStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link FloatStruct#valueOf(Apfloat)} method.
	 */
	private static final String FLOAT_STRUCT_VALUE_OF_METHOD_NAME = "valueOf";

	/**
	 * Constant {@link String} containing the description for the {@link FloatStruct#valueOf(Apfloat)} method.
	 */
	private static final String FLOAT_STRUCT_VALUE_OF_METHOD_DESC
			= CodeGenerators.getMethodDescription(FloatStruct.class, FLOAT_STRUCT_VALUE_OF_METHOD_NAME, Apfloat.class);

	/**
	 * Constant {@link String} containing the name for the {@link Apfloat} class.
	 */
	private static final String APFLOAT_NAME = Type.getInternalName(Apfloat.class);

	/**
	 * Constant {@link String} containing the description for the {@link Apfloat#Apfloat(String)} constructor method.
	 */
	private static final String APFLOAT_INIT_METHOD_DESC
			= CodeGenerators.getConstructorDescription(Apfloat.class, String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link FloatStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Constructing a new {@link Apfloat} from the {@link String} representation of the {@link FloatStruct#ap}
	 * value</li>
	 * <li>Retrieving a {@link FloatStruct} via {@link FloatStruct#valueOf(Apfloat)} with the created {@link Apfloat}
	 * value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link FloatStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<FloatStruct> event) {
		final FloatStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, APFLOAT_NAME);
		mv.visitInsn(Opcodes.DUP);

		final String apString = input.ap().toString();
		mv.visitLdcInsn(apString);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   APFLOAT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   APFLOAT_INIT_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   FLOAT_STRUCT_NAME,
		                   FLOAT_STRUCT_VALUE_OF_METHOD_NAME,
		                   FLOAT_STRUCT_VALUE_OF_METHOD_DESC,
		                   false);
	}
}
