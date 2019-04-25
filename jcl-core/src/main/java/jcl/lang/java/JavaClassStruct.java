/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.LispStructImpl;

public class JavaClassStruct extends LispStructImpl {

	private final Class<?> javaClass;

	private JavaClassStruct(final Class<?> javaClass) {
		this.javaClass = javaClass;
	}

	public Class<?> getJavaClass() {
		return javaClass;
	}

	public static JavaClassStruct toJavaClass(final Class<?> javaClass) {
		return new JavaClassStruct(javaClass);
	}

	public static JavaClassStruct toJavaClass(final String className) {
		try {
			final Class<?> javaClass = Class.forName(className);
			return new JavaClassStruct(javaClass);
		} catch (final ClassNotFoundException ex) {
			throw new ErrorException("Java Class not found for class name '" + className + "'.", ex);
		}
	}
}
