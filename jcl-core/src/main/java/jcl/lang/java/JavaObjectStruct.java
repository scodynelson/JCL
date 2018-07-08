/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import jcl.lang.classes.BuiltInClassStruct;

public class JavaObjectStruct extends BuiltInClassStruct {

	private final Object javaObject;

	private JavaObjectStruct(final Object javaObject) {
		super(null, null);

		this.javaObject = javaObject;
	}

	public static JavaObjectStruct valueOf(final Object javaObject) {
		return new JavaObjectStruct(javaObject);
	}

	public Object getJavaObject() {
		return javaObject;
	}

	@Override
	public String toString() {
		return javaObject.toString();
	}
}
