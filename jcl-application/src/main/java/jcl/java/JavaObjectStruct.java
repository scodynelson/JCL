/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java;

import jcl.lang.BuiltInClassStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class JavaObjectStruct extends BuiltInClassStruct {

	private final Object javaObject;

	public JavaObjectStruct(final Object javaObject) {
		super(null, null);

		this.javaObject = javaObject;
	}

	public Object getJavaObject() {
		return javaObject;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(javaObject)
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
		final JavaObjectStruct rhs = (JavaObjectStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(javaObject, rhs.javaObject)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return javaObject.toString();
	}
}
