/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SpecialVariableCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final String symbolName = input.getName();
		final PackageStruct symbolPackage = input.getSymbolPackage();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		if (symbolPackage != null) {
			final String packageName = symbolPackage.getName();
			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)Llisp/common/type/Package;", false);
		}

		mv.visitLdcInsn(symbolName);

		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/Package", "intern", "(Ljava/lang/String;)[Llisp/common/type/Symbol;", true);
		mv.visitLdcInsn(0);
		mv.visitInsn(Opcodes.AALOAD);
	}
}
