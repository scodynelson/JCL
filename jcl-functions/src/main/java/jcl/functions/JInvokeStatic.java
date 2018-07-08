/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.lang.reflect.Method;
import java.util.List;

import jcl.compiler.function.InternalEval;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.java.JavaObjectStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class JInvokeStatic extends ExtensionsBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "JINVOKE-STATIC";
	private static final String JAVA_METHOD_ARGUMENT = "JAVA-METHOD";

	@Autowired
	private InternalEval internalEval;

	public JInvokeStatic() {
		super("Invokes a static method for the provided JavaMethod",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(JAVA_METHOD_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final JavaMethodStruct javaMethodStruct
				= arguments.getRequiredArgument(JAVA_METHOD_ARGUMENT, JavaMethodStruct.class);
		final Method javaMethod = javaMethodStruct.getJavaMethod();

		final List<LispStruct> args = arguments.getRestArgument();
		final Object[] methodArgs = new Object[args.size()];
		for (int i = 0; i < args.size(); i++) {
			final LispStruct currentArg = args.get(i);
			methodArgs[i] = unwrapJavaObject(currentArg);
		}

		return internalEval.jInvoke(javaMethod, null, methodArgs);
	}

	private static Object unwrapJavaObject(final Object javaObject) {
		if (javaObject instanceof JavaObjectStruct) {
			return ((JavaObjectStruct) javaObject).getJavaObject();
		}
		return javaObject;
	}
}
