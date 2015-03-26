/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;

public class SpecialVariableCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		// push current package
		emitSymbolPackage(input, classBuilder);
		classBuilder.getEmitter().emitLdc(input.getName());
		// invoke package.intern() - we may not have seen it before
		classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Package", "intern", "(Ljava/lang/String;)", "[Llisp/common/type/Symbol;", true);
		classBuilder.getEmitter().emitLdc(0);
		classBuilder.getEmitter().emitAaload();
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym
	 * 		lisp.common.type.Sybmbol sym
	 * @param classBuilder
	 * 		classBuilder
	 *
	 * @return object
	 */
	public static Object emitSymbolPackage(final SymbolStruct<?> sym, final JavaClassBuilder classBuilder) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			final PackageStruct homePkgName = sym.getSymbolPackage();
			emitPackage(homePkgName.getName(), classBuilder);
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
	 * @param classBuilder
	 * 		classBuilder
	 *
	 * @return object
	 */
	private static Object emitPackage(final String name, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitLdc(name);
		//String owner, String name, String descr
		classBuilder.getEmitter().emitInvokestatic("lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)", "Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}
}
