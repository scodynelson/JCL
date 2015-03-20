/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public class SpecialVariableCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		if (input.equals(NILStruct.INSTANCE)) {
			classBuilder.getEmitter().emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
		} else if (input.equals(TStruct.INSTANCE)) {
			classBuilder.getEmitter().emitGetstatic("jcl/symbols/TStruct", "INSTANCE", "Ljcl/symbols/TStruct;");
		} else {
			// push current package
			emitSymbolPackage(input, classBuilder);
			classBuilder.getEmitter().emitLdc(input.getName());
			// invoke package.intern() - we may not have seen it before
			classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Package", "intern", "(Ljava/lang/String;)", "[Llisp/common/type/Symbol;", true);
			classBuilder.getEmitter().emitLdc(0);
			classBuilder.getEmitter().emitAaload();
		}
	}

	/**
	 * Emitter method for Java function EMIT-SYMBOL-PACKAGE and class for Lisp
	 * function for new Lisp compiler.
	 *
	 * @param sym
	 * 		lisp.common.type.Sybmbol sym
	 * @param classBuilder classBuilder
	 * @return object
	 */
	public static Object emitSymbolPackage(final SymbolStruct<?> sym, final JavaClassBuilder classBuilder) {
		// There are optimizations for the standard packages
		if (sym.getSymbolPackage() != null) {
			final PackageStruct homePkgName = sym.getSymbolPackage();
			if (homePkgName.equals(GlobalPackageStruct.COMMON_LISP)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "CommonLisp", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.COMMON_LISP_USER)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "CommonLispUser", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.KEYWORD)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "Keyword", "Llisp/common/type/Package;");
			} else if (homePkgName.equals(GlobalPackageStruct.SYSTEM)) {
				classBuilder.getEmitter().emitGetstatic("lisp/common/type/Package", "System", "Llisp/common/type/Package;");
			} else {
				emitPackage(homePkgName.getName(), classBuilder);
			}
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
	 * @param classBuilder classBuilder
	 * @return object
	 */
	private static Object emitPackage(final String name, final JavaClassBuilder classBuilder) {
//        Label label = new Label();
//        visitMethodLabel(label);
//        emitLine(++LineNumber, label);
		classBuilder.getEmitter().emitLdc(name);
		//String owner, String name, String descr
		classBuilder.getEmitter().emitInvokestatic("lisp/system/PackageImpl", "findPackage", "(Ljava/lang/String;)", "Llisp/common/type/Package;", false);
		return NILStruct.INSTANCE;
	}
}
