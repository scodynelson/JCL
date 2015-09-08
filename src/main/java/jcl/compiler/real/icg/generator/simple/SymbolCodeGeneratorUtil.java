/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.arrays.StringStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public final class SymbolCodeGeneratorUtil {

	private static final String SYMBOL_STRUCT_INIT_STRING_DESC = GeneratorUtils.getConstructorDescription(StringStruct.class, String.class);

	private SymbolCodeGeneratorUtil() {
	}

	public static void generate(final SymbolStruct<?> input, final GeneratorState classBuilder,
	                            final int packageStore, final int symbolStore) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final PackageStruct pkg = input.getSymbolPackage();
		final String symbolName = input.getName();

		if (pkg == null) {
			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.SYMBOL_STRUCT_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					SYMBOL_STRUCT_INIT_STRING_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);
		} else {
			final String packageName = pkg.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
					GenerationConstants.PACKAGE_STRUCT_NAME,
					GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
					GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
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
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);
		}
	}
}
