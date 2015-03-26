/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SpecialVariableCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();


		// push current package
		emitSymbolPackage(input, mv);
		mv.visitLdcInsn(input.getName());
		// invoke package.intern() - we may not have seen it before
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/Package", "intern", "(Ljava/lang/String;)[Llisp/common/type/Symbol;", true);
		mv.visitLdcInsn(0);
		mv.visitInsn(Opcodes.AALOAD);
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym
	 * 		lisp.common.type.Sybmbol sym
	 * @param mv
	 * 		classBuilder
	 *
	 * @return object
	 */
	public static Object emitSymbolPackage(final SymbolStruct<?> sym, final MethodVisitor mv) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			final PackageStruct homePkgName = sym.getSymbolPackage();
			emitPackage(homePkgName.getName(), mv);
		} else {
			// no package
		}
		return NILStruct.INSTANCE;
	}

	/**
	 * Emitter method for Java function EMIT-PACKAGE and class for Lisp function
	 * for new Lisp compiler.
	 *
	 * @param name
	 * 		lisp.common.type.Package name
	 * @param mv
	 * 		classBuilder
	 *
	 * @return object
	 */
	private static Object emitPackage(final String name, final MethodVisitor mv) {
		mv.visitLdcInsn(name);
		//String owner, String name, String descr
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}
}
