/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.statics.CommonLispSymbols;

public final class JClass extends BuiltInFunctionStructImpl {

	private static final String CLASS_NAME_ARGUMENT = "CLASS_NAME";

	public JClass() {
		super("Gets the Java class matching the provided class name string.",
		      CommonLispSymbols.JCLASS.getName(),
		      Parameters.forFunction(CommonLispSymbols.JCLASS.getName())
		                .requiredParameter(CLASS_NAME_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.JCLASS;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct className = arguments.getRequiredArgument(CLASS_NAME_ARGUMENT, StringStruct.class);
		final String classNameString = className.toJavaString();
		return JavaClassStruct.toJavaClass(classNameString);
	}
}
