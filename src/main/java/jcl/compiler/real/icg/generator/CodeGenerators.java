/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import org.objectweb.asm.Type;

final class CodeGenerators {

	private CodeGenerators() {
	}

	static <TYPE> String getConstructorDescription(final Class<TYPE> clazz, final Class<?>... parameterTypes) {
		final Constructor<TYPE> constructor = getConstructor(clazz, parameterTypes);
		if (constructor == null) {
			return null;
		} else {
			return Type.getConstructorDescriptor(constructor);
		}
	}

	private static <TYPE> Constructor<TYPE> getConstructor(final Class<TYPE> clazz, final Class<?>... parameterTypes) {
		try {
			return clazz.getDeclaredConstructor(parameterTypes);
		} catch (final NoSuchMethodException ignored) {
			return null;
		}
	}

	static <TYPE> String getMethodDescription(final Class<TYPE> clazz, final String methodName, final Class<?>... parameterTypes) {
		final Method method = getMethod(clazz, methodName, parameterTypes);
		if (method == null) {
			return null;
		} else {
			return Type.getMethodDescriptor(method);
		}
	}

	private static Method getMethod(final Class<?> clazz, final String methodName, final Class<?>... parameterTypes) {
		try {
			return clazz.getMethod(methodName, parameterTypes);
		} catch (final NoSuchMethodException ignored) {
			return null;
		}
	}
}
