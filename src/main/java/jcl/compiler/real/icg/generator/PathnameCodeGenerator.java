/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.net.URI;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.pathnames.PathnameStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
class PathnameCodeGenerator implements CodeGenerator<PathnameStruct> {

	private static final String PATHNAME_STRUCT_NAME = Type.getInternalName(PathnameStruct.class);

	private static final String PATHNAME_STRUCT_NAME_INIT_DESC = GeneratorUtils.getConstructorDescription(PathnameStruct.class, URI.class);

	@Override
	public void generate(final PathnameStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final URI uri = input.getUri();
		final String filePath = uri.toString();

		mv.visitLdcInsn(filePath);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
				GenerationConstants.JAVA_URI_NAME,
				GenerationConstants.JAVA_URI_CREATE_METHOD_NAME,
				GenerationConstants.JAVA_URI_CREATE_METHOD_DESC,
				false);
		final int uriStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, uriStore);

		mv.visitTypeInsn(Opcodes.NEW, PATHNAME_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, uriStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				PATHNAME_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				PATHNAME_STRUCT_NAME_INIT_DESC,
				false);
	}
}
