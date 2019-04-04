package jcl.lang.factory;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.java.JavaNameStruct;
import jcl.lang.java.JavaObjectStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class LispStructFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(LispStructFactory.class);

	private LispStructFactory() {
	}

	/*
	 * JavaClassStruct
	 */

	public static JavaClassStruct toJavaClass(final String className) {
		try {
			final Class<?> javaClass = Class.forName(className);
			return JavaClassStruct.valueOf(javaClass);
		} catch (final ClassNotFoundException ex) {
			throw new ErrorException("Java Class not found for class name '" + className + "'.", ex);
		}
	}

	/*
	 * JavaMethodStruct
	 */

	public static Method toJavaReflectionMethod(final String methodName,
	                                            final Class<?> javaClass,
	                                            final Class<?>... parameterTypes) {
		final String javaClassName = javaClass.getName();
		final Method[] methods = javaClass.getMethods();

		Method matchingMethod = null;

		for (final Method method : methods) {
			final String currentMethodName = method.getName();
			if (!currentMethodName.equals(methodName)) {
				continue;
			}
			final Class<?>[] currentParamTypes = method.getParameterTypes();
			if (currentParamTypes.length != parameterTypes.length) {
				continue;
			}

			boolean matches = true;
			for (int i = 0; i < currentParamTypes.length; i++) {
				final Class<?> currentParamType = currentParamTypes[i];
				final Class<?> expectedParamType = parameterTypes[i];

				if (!currentParamType.isAssignableFrom(expectedParamType)) {
					matches = false;
				}
			}
			if (matches) {
				matchingMethod = method;
				break;
			}
		}
		if (matchingMethod == null) {
			throw new ErrorException(
					"Java Class '" + javaClassName +
							"' does not have the method '" + methodName +
							"' with parameter types '" + Arrays.toString(parameterTypes) +
							"'.");
		}
		return matchingMethod;
	}

	public static JavaMethodStruct toJavaMethod(final String methodName,
	                                            final Class<?> javaClass,
	                                            final Class<?>... parameterTypes) {
		return JavaMethodStruct.valueOf(toJavaReflectionMethod(methodName, javaClass, parameterTypes));
	}

	/*
	 * JavaNameStruct
	 */

	public static JavaNameStruct toJavaName(final String javaName) {
		return JavaNameStruct.valueOf(javaName);
	}

	/*
	 * JavaObjectStruct
	 */

	public static JavaObjectStruct toJavaObject(final Class<?> javaClass) {
		final String javaClassName = javaClass.getName();
		try {
			final Constructor<?> defaultConstructor = javaClass.getDeclaredConstructor();
			final Object newInstance = defaultConstructor.newInstance();
			return JavaObjectStruct.valueOf(newInstance);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException(
					"Java Class '" + javaClassName +
							"' does not have a default no argument constructor.", ex);
		} catch (final InvocationTargetException | InstantiationException | IllegalAccessException ex) {
			final String message = "Java Class '" + javaClassName + "' could not be instantiated.";
			LOGGER.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}
}
