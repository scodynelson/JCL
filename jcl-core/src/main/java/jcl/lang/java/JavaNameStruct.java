/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import jcl.lang.classes.BuiltInClassStruct;

public class JavaNameStruct extends BuiltInClassStruct {

	private final String javaName;

	private JavaNameStruct(final String javaName) {
		super(null, null);

		this.javaName = javaName;
	}

	public static JavaNameStruct valueOf(final String javaName) {
		return new JavaNameStruct(javaName);
	}

	public String getJavaName() {
		return javaName;
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		return javaName;
	}
}
