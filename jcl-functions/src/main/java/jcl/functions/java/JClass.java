/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import jcl.lang.LispStruct;
import jcl.lang.array.StringStructImpl;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.ExtensionsBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.java.JavaClassStruct;
import org.springframework.stereotype.Component;

@Component
public final class JClass extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "JCLASS";
	private static final String CLASS_NAME_ARGUMENT = "CLASS_NAME";

	public JClass() {
		super("Gets the Java class matching the provided class name string.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CLASS_NAME_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStructImpl className = arguments.getRequiredArgument(CLASS_NAME_ARGUMENT, StringStructImpl.class);
		return jClass(className.getAsJavaString());
	}

	public JavaClassStruct jClass(final String className) {
		try {
			final Class<?> javaClass = Class.forName(className);
			return new JavaClassStruct(javaClass);
		} catch (final ClassNotFoundException ex) {
			throw new ErrorException("Java Class not found for class name '" + className + "'.", ex);
		}
	}
}
