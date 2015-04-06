/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public final class SymbolCodeGeneratorUtil {

	public static int generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final String packageName = input.getSymbolPackage().getName();
		final String symbolName = input.getName();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
		final int packageStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		final int symbolStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, symbolStore);

		return symbolStore;
	}
}
