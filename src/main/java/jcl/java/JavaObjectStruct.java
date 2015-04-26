/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java;

import jcl.classes.BuiltInClassStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class JavaObjectStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = -1994392358840879592L;

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
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(javaObject)
		                                                                .toString();
	}
}