/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.java;

import jcl.lang.internal.BuiltInClassStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class JavaNameStruct extends BuiltInClassStruct {

	private final String javaName;

	public JavaNameStruct(final String javaName) {
		super(null, null);

		this.javaName = javaName;
	}

	public String getJavaName() {
		return javaName;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(javaName)
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
		final JavaNameStruct rhs = (JavaNameStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(javaName, rhs.javaName)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(javaName)
		                                                                .toString();
	}
}
