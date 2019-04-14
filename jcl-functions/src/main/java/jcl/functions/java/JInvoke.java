/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import java.lang.reflect.Method;
import java.util.List;

import jcl.compiler.function.InternalEval;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.java.JavaObjectStruct;
import jcl.lang.statics.CommonLispSymbols;

public final class JInvoke extends BuiltInFunctionStructImpl {

	private static final String JAVA_METHOD_ARGUMENT = "JAVA-METHOD";
	private static final String JAVA_OBJECT_ARGUMENT = "JAVA-OBJECT";

	public JInvoke() {
		super("Invokes an instance method for the provided JavaMethod and JavaObject.",
		      CommonLispSymbols.JINVOKE.getName(),
		      Parameters.forFunction(CommonLispSymbols.JINVOKE.getName())
		                .requiredParameter(JAVA_METHOD_ARGUMENT)
		                .requiredParameter(JAVA_OBJECT_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.JINVOKE;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final JavaMethodStruct javaMethodStruct
				= arguments.getRequiredArgument(JAVA_METHOD_ARGUMENT, JavaMethodStruct.class);
		final Method javaMethod = javaMethodStruct.getJavaMethod();

		final Object javaObject = unwrapJavaObject(
				arguments.getRequiredArgument(JAVA_OBJECT_ARGUMENT)
		);

		final List<LispStruct> args = arguments.getRestArgument();
		final Object[] methodArgs = new Object[args.size()];
		for (int i = 0; i < args.size(); i++) {
			final LispStruct currentArg = args.get(i);
			methodArgs[i] = unwrapJavaObject(currentArg);
		}

		return InternalEval.jInvoke(javaMethod, javaObject, methodArgs);
	}

	private static Object unwrapJavaObject(final Object javaObject) {
		if (javaObject instanceof JavaObjectStruct) {
			return ((JavaObjectStruct) javaObject).getJavaObject();
		}
		return javaObject;
	}
}
