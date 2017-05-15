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
public class JInvoke extends ExtensionsBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "JINVOKE";
	private static final String JAVA_METHOD_ARGUMENT = "JAVA-METHOD";
	private static final String JAVA_OBJECT_ARGUMENT = "JAVA-OBJECT";

	@Autowired
	private InternalEval internalEval;

	public JInvoke() {
		super("Creates a new instance of the Java Class matching the provided string",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(JAVA_METHOD_ARGUMENT)
		                .requiredParameter(JAVA_OBJECT_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final JavaMethodStruct javaMethodStruct = arguments.getRequiredArgument(JAVA_METHOD_ARGUMENT, JavaMethodStruct.class);
		final Method javaMethod = javaMethodStruct.getJavaMethod();

		final JavaObjectStruct javaObjectStruct = arguments.getRequiredArgument(JAVA_OBJECT_ARGUMENT, JavaObjectStruct.class);
		final Object javaObject = javaObjectStruct.getJavaObject();

		final List<LispStruct> args = arguments.getRestArgument();
		final LispStruct[] methodArgs = new LispStruct[args.size()];
		for (int i = 0; i < args.size(); i++) {
			final LispStruct currentArg = args.get(i);
			methodArgs[i] = currentArg;
		}

		return internalEval.jInvoke(javaMethod, javaObject, methodArgs);
	}
}
