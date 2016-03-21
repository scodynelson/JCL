/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java.functions;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.ExtensionsBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.java.JavaClassStruct;
import jcl.java.JavaObjectStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public final class JNew extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "JNEW";
	private static final String JAVA_CLASS_ARGUMENT = "JAVA-CLASS";

	private static final Logger LOGGER = LoggerFactory.getLogger(JNew.class);

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
		return jNew(javaClassStruct.getJavaClass());
	}

	public JavaObjectStruct jNew(final Class<?> javaClass) {
		final String javaClassName = javaClass.getName();
		try {
			final Constructor<?> defaultConstructor = javaClass.getDeclaredConstructor();
			final Object newInstance = defaultConstructor.newInstance();
			return new JavaObjectStruct(newInstance);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException("Java Class '" + javaClassName + "' does not have a default no argument constructor.", ex);
		} catch (final InvocationTargetException | InstantiationException | IllegalAccessException ex) {
			final String message = "Java Class '" + javaClassName + "' could not be instantiated.";
			LOGGER.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}
}
