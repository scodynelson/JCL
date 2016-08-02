/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.ComplexStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.ComplexStructImpl;
import org.apfloat.Apcomplex;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to generate {@link ComplexStruct} objects dynamically by utilizing the {@link ComplexStruct#ap()} and {@link
 * ComplexStruct#getValueType()} of the provided {@link ComplexStruct} input value.
 */
@Component
final class ComplexCodeGenerator implements CodeGenerator<ComplexStructImpl> {

	/**
	 * Constant {@link String} containing the name for the {@link ComplexStruct.ValueType} class.
	 */
	private static final String VALUE_TYPE_NAME = Type.getInternalName(ComplexStruct.ValueType.class);

	/**
	 * Constant {@link String} containing the descriptor for the {@link ComplexStruct.ValueType} class.
	 */
	private static final String VALUE_TYPE_DESC = Type.getDescriptor(ComplexStruct.ValueType.class);

	/**
	 * Constant {@link String} containing the name for the {@link Apcomplex} class.
	 */
	private static final String APCOMPLEX_NAME = Type.getInternalName(Apcomplex.class);

	/**
	 * Constant {@link String} containing the description for the {@link Apcomplex#Apcomplex(String)} constructor
	 * method.
	 */
	private static final String APCOMPLEX_INIT_METHOD_DESC
			= CodeGenerators.getConstructorDescription(Apcomplex.class, String.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ComplexStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Constructing a new {@link Apcomplex} from the {@link String} representation of the {@link ComplexStruct#ap}
	 * value</li>
	 * <li>Loading the {@link ComplexStruct.ValueType} from the provided {@link ComplexStruct#getValueType()}
	 * value</li>
	 * <li>Retrieving a {@link ComplexStruct} via {@link LispStructFactory#toComplex(Apcomplex,
	 * ComplexStruct.ValueType)} with
	 * the created {@link Apcomplex} and loaded {@link ComplexStruct.ValueType} values</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link ComplexStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<ComplexStructImpl> event) {
		final ComplexStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, APCOMPLEX_NAME);
		mv.visitInsn(Opcodes.DUP);

		final String apString = input.ap().toString();
		mv.visitLdcInsn(apString);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   APCOMPLEX_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   APCOMPLEX_INIT_METHOD_DESC,
		                   false);
		final int apComplexStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, apComplexStore);

		final ComplexStruct.ValueType valueType = input.getValueType();
		final String name = valueType.name();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  VALUE_TYPE_NAME,
		                  name,
		                  VALUE_TYPE_DESC);
		final int valueTypeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valueTypeStore);

		mv.visitVarInsn(Opcodes.ALOAD, apComplexStore);
		mv.visitVarInsn(Opcodes.ALOAD, valueTypeStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_COMPLEX_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_COMPLEX_METHOD_DESC,
		                   false);
	}
}
