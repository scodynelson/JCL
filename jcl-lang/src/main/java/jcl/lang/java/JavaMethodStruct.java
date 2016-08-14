/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import java.lang.reflect.Method;

import jcl.lang.internal.BuiltInClassStruct;

public class JavaMethodStruct extends BuiltInClassStruct {

	private final Method javaMethod;

	private JavaMethodStruct(final Method javaMethod) {
		super(null, null);

		this.javaMethod = javaMethod;
	}

	public static JavaMethodStruct valueOf(final Method javaMethod) {
		return new JavaMethodStruct(javaMethod);
	}

	public Method getJavaMethod() {
		return javaMethod;
	}
}
