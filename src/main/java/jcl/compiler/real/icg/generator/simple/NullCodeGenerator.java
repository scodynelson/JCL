/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.lists.NullStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class NullCodeGenerator implements CodeGenerator<NullStruct> {

	private static final String NULL_STRUCT_NAME = Type.getInternalName(NullStruct.class);

	private static final String NULL_STRUCT_DESC = Type.getDescriptor(NullStruct.class);

	@Override
	public void generate(final NullStruct input, final JavaClassBuilder classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, NULL_STRUCT_NAME, GenerationConstants.SINGLETON_INSTANCE, NULL_STRUCT_DESC);
	}
}
