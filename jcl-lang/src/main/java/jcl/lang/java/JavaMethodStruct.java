/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import java.lang.reflect.Method;

import jcl.lang.BuiltInClassStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class JavaMethodStruct extends BuiltInClassStruct {

	private final Method javaMethod;

	public JavaMethodStruct(final Method javaMethod) {
		super(null, null);

		this.javaMethod = javaMethod;
	}

	public Method getJavaMethod() {
		return javaMethod;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(javaMethod)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final JavaMethodStruct rhs = (JavaMethodStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(javaMethod, rhs.javaMethod)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return javaMethod.toString();
	}
}