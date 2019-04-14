/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.java.JavaObjectStruct;
import jcl.lang.statics.CommonLispSymbols;

public final class JNew extends BuiltInFunctionStructImpl {

	private static final String JAVA_CLASS_ARGUMENT = "JAVA-CLASS";

	public JNew() {
		super("Creates a new instance of the Java Class matching the provided string",
		      CommonLispSymbols.JNEW.getName(),
		      Parameters.forFunction(CommonLispSymbols.JNEW.getName())
		                .requiredParameter(JAVA_CLASS_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.JNEW;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final JavaClassStruct javaClassStruct = arguments.getRequiredArgument(JAVA_CLASS_ARGUMENT, JavaClassStruct.class);
		final Class<?> javaClass = javaClassStruct.getJavaClass();
		return JavaObjectStruct.toJavaObject(javaClass);
	}
}
