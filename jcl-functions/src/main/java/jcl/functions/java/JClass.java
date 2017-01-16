/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.java;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class JClass extends ExtensionsBuiltInFunctionStructBase {

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
		final StringStruct className = arguments.getRequiredArgument(CLASS_NAME_ARGUMENT, StringStruct.class);
		final String classNameString = className.toJavaString();
		return LispStructFactory.toJavaClass(classNameString);
	}
}
