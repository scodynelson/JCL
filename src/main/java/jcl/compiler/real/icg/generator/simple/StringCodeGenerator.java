/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.arrays.StringStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class StringCodeGenerator implements CodeGenerator<StringStruct> {

	@Override
	public void generate(final StringStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, "jcl/arrays/StringStruct");
		mv.visitInsn(Opcodes.DUP);

		final String javaString = input.getAsJavaString();
		mv.visitLdcInsn(javaString);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/arrays/StringStruct", "<init>", "(Ljava/lang/String;)V", false);
	}
}
