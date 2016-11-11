/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import jcl.lang.classes.BuiltInClassStruct;

public class JavaClassStruct extends BuiltInClassStruct {

	private final Class<?> javaClass;

	private JavaClassStruct(final Class<?> javaClass) {
		super(null, null);

		this.javaClass = javaClass;
	}

	public static JavaClassStruct valueOf(final Class<?> javaClass) {
		return new JavaClassStruct(javaClass);
	}

	public Class<?> getJavaClass() {
		return javaClass;
	}
}
