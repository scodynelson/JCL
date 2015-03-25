/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class ImmutableLoadTimeValueCodeGenerator implements CodeGenerator<ImmutableLoadTimeValueStruct> {

	@Override
	public void generate(final ImmutableLoadTimeValueStruct input, final JavaClassBuilder classBuilder) {

		final String uniqueLTVId = input.getUniqueLTVId();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final String className = currentClass.getName();

		mv.visitFieldInsn(Opcodes.GETSTATIC, className, uniqueLTVId, "Ljcl/LispStruct;");
	}
}
