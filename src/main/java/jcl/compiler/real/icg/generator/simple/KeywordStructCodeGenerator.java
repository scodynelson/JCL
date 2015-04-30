/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class KeywordStructCodeGenerator implements CodeGenerator<KeywordStruct> {

	private static final String GLOBAL_PACKAGE_STRUCT_NAME = Type.getInternalName(GlobalPackageStruct.class);

	private static final String KEYWORD_PACKAGE_NAME = "KEYWORD";

	private static final String KEYWORD_STRUCT_NAME = Type.getInternalName(KeywordStruct.class);

	@Override
	public void generate(final KeywordStruct input, final JavaClassBuilder classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, GLOBAL_PACKAGE_STRUCT_NAME, KEYWORD_PACKAGE_NAME, GenerationConstants.PACKAGE_STRUCT_DESC);
		final int packageStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		final String keywordName = input.getName();

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		mv.visitLdcInsn(keywordName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.PACKAGE_STRUCT_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_SYMBOL_METHOD_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_SYMBOL_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_NAME,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC,
				false);
		mv.visitTypeInsn(Opcodes.CHECKCAST, KEYWORD_STRUCT_NAME);
	}
}
