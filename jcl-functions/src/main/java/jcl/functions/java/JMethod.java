/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import java.util.List;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.java.JavaMethodStruct;
import org.springframework.stereotype.Component;

@Component
public final class JMethod extends ExtensionsBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "JMETHOD";
	private static final String METHOD_NAME_ARGUMENT = "METHOD-NAME";
	private static final String JAVA_CLASS_ARGUMENT = "JAVA-CLASS";

	public JMethod() {
		super("Gets the Java method matching the provided method name string for the provided Java Class object and the provided Java Class parameter object types.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(METHOD_NAME_ARGUMENT)
		                .requiredParameter(JAVA_CLASS_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final StringStruct methodName = arguments.getRequiredArgument(METHOD_NAME_ARGUMENT, StringStruct.class);
		final String methodNameString = methodName.toJavaString();

		final JavaClassStruct javaClassStruct = arguments.getRequiredArgument(JAVA_CLASS_ARGUMENT, JavaClassStruct.class);
		final Class<?> javaClass = javaClassStruct.getJavaClass();

		final List<LispStruct> args = arguments.getRestArgument();
		final Class<?>[] parameterTypes = new Class<?>[args.size()];
		for (int i = 0; i < args.size(); i++) {
			final LispStruct currentArg = args.get(i);
			final JavaClassStruct methodParamClassStruct = (JavaClassStruct) currentArg;
			parameterTypes[i] = methodParamClassStruct.getJavaClass();
		}

		return JavaMethodStruct.toJavaMethod(methodNameString, javaClass, parameterTypes);
	}
}
