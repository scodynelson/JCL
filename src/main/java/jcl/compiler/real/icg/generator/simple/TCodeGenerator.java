/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.symbols.TStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class TCodeGenerator implements CodeGenerator<TStruct> {

	private static final String T_STRUCT_NAME = Type.getInternalName(TStruct.class);

	private static final String T_STRUCT_DESC = Type.getDescriptor(TStruct.class);

	@Override
	public void generate(final TStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, T_STRUCT_NAME, GenerationConstants.SINGLETON_INSTANCE, T_STRUCT_DESC);
	}
}
