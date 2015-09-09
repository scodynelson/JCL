/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
class ImmutableLoadTimeValueCodeGenerator implements CodeGenerator<ImmutableLoadTimeValueStruct> {

	@Override
	public void generate(final ImmutableLoadTimeValueStruct input, final GeneratorState generatorState) {

		final String uniqueLTVId = input.getUniqueLTVId();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String fileName = currentClass.getFileName();

		mv.visitFieldInsn(Opcodes.GETSTATIC, fileName, uniqueLTVId, GenerationConstants.LISP_STRUCT_DESC);
	}
}
