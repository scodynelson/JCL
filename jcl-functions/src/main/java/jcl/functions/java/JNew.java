/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaClassStruct;
import org.springframework.stereotype.Component;

@Component
public final class JNew extends ExtensionsBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "JNEW";
	private static final String JAVA_CLASS_ARGUMENT = "JAVA-CLASS";

	public JNew() {
		super("Creates a new instance of the Java Class matching the provided string",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(JAVA_CLASS_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final JavaClassStruct javaClassStruct = arguments.getRequiredArgument(JAVA_CLASS_ARGUMENT, JavaClassStruct.class);
		final Class<?> javaClass = javaClassStruct.getJavaClass();
		return LispStructFactory.toJavaObject(javaClass);
	}
}
