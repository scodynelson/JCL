/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.classes.BuiltInClassStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class JavaNameStruct extends BuiltInClassStruct {

	private final String javaName;

	private JavaNameStruct(final String javaName) {
		super(null, null);

		this.javaName = javaName;
	}

	public String getJavaName() {
		return javaName;
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitLdcInsn(javaName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.JAVA_NAME_STRUCT_NAME,
		                   GenerationConstants.JAVA_NAME_STRUCT_TO_JAVA_NAME_METHOD_NAME,
		                   GenerationConstants.JAVA_NAME_STRUCT_TO_JAVA_NAME_METHOD_DESC,
		                   false);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		return javaName;
	}

	public static JavaNameStruct toJavaName(final String javaName) {
		return new JavaNameStruct(javaName);
	}
}
