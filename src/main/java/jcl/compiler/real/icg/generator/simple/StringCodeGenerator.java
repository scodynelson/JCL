/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.arrays.StringStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.compiler.real.icg.generator.GenerationConstants;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class StringCodeGenerator implements CodeGenerator<StringStruct> {

	private static final String STRING_STRUCT_NAME = Type.getInternalName(StringStruct.class);

	private static final String STRING_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(StringStruct.class, String.class);

	@Override
	public void generate(final StringStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, STRING_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		final String javaString = input.getAsJavaString();
		mv.visitLdcInsn(javaString);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, STRING_STRUCT_NAME, GenerationConstants.INIT_METHOD_NAME, STRING_STRUCT_INIT_DESC, false);
	}
}
