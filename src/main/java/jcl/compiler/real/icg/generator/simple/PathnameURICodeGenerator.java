/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import java.net.URI;
import java.nio.file.Path;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.pathnames.PathnameURIStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class PathnameURICodeGenerator implements CodeGenerator<PathnameURIStruct> {

	private static final String PATHNAME_URI_STRUCT_NAME = Type.getInternalName(PathnameURIStruct.class);

	private static final String PATHNAME_URI_STRUCT_NAME_INIT_DESC = GeneratorUtils.getConstructorDescription(PathnameURIStruct.class, Path.class);

	@Override
	public void generate(final PathnameURIStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Path path = input.getPath();
		final URI uri = path.toUri();
		final String uriPath = uri.getPath();

		mv.visitLdcInsn(uriPath);
		mv.visitInsn(Opcodes.ICONST_0);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.JAVA_STRING_NAME);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
				GenerationConstants.JAVA_PATHS_NAME,
				GenerationConstants.JAVA_PATHS_GET_METHOD_NAME,
				GenerationConstants.JAVA_PATHS_GET_METHOD_DESC,
				false);
		final int pathStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, pathStore);

		mv.visitTypeInsn(Opcodes.NEW, PATHNAME_URI_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, pathStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				PATHNAME_URI_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				PATHNAME_URI_STRUCT_NAME_INIT_DESC,
				false);
	}
}
