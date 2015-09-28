/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'load-time-value' special operator code generation where the value is immutable, or read-only.
 */
@Component
class ImmutableLoadTimeValueCodeGenerator implements CodeGenerator<ImmutableLoadTimeValueStruct> {

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ImmutableLoadTimeValueStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving the pre-generated form value calculated at load-time (in this case, the field with the name
	 * 'f2ceebd1_e143_4b7f_86ca_96a7835275f0'</li>
	 * </ol>
	 * As an example, it will transform {@code (load-time-value 1 t)} into the following Java code:
	 * <pre>
	 * {@code
	 *      val2 = f2ceebd1_e143_4b7f_86ca_96a7835275f0;
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link ImmutableLoadTimeValueStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final ImmutableLoadTimeValueStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final JavaClassBuilder currentClassBuilder = generatorState.getCurrentClassBuilder();
		final String className = currentClassBuilder.getClassName();

		final String uniqueLTVId = input.getUniqueLTVId();
		mv.visitVarInsn(Opcodes.ALOAD, 0); // 'this' store
		mv.visitFieldInsn(Opcodes.GETFIELD, className, uniqueLTVId, GenerationConstants.LISP_STRUCT_DESC);
	}
}
