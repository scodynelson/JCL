/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import java.lang.reflect.Method;
import java.util.Arrays;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.LispStructImpl;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
public class JavaMethodStruct extends LispStructImpl {

	private final Method javaMethod;

	public static JavaMethodStruct toJavaMethod(final Method javaMethod) {
		return new JavaMethodStruct(javaMethod);
	}

	public static JavaMethodStruct toJavaMethod(final String methodName, final Class<?> javaClass,
	                                            final Class<?>... parameterTypes) {
		final Method javaMethod = getMethod(methodName, javaClass, parameterTypes);
		return new JavaMethodStruct(javaMethod);
	}

	private static Method getMethod(final String methodName, final Class<?> javaClass,
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

	@Override
	public String toString() {
		return "#<" + javaMethod + '>';
	}
}
