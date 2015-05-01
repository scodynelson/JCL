/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public final class SymbolCodeGeneratorUtil {

	private SymbolCodeGeneratorUtil() {
	}

	public static void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder,
	                            final int packageStore, final int symbolStore) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final PackageStruct pkg = input.getSymbolPackage();
		final String symbolName = input.getName();

		if (pkg == null) {
			mv.visitTypeInsn(Opcodes.NEW, "jcl/symbols/SymbolStruct");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/symbols/SymbolStruct", "<init>", "(Ljava/lang/String;)V", false);
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
