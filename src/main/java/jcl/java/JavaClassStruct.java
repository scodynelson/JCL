/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.java;

import jcl.classes.BuiltInClassStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class JavaClassStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = 7866392027616931608L;

	private final Class<?> javaClass;

	public JavaClassStruct(final Class<?> javaClass) {
		super(null, null);

		this.javaClass = javaClass;
	}

	public Class<?> getJavaClass() {
		return javaClass;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(javaClass)
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
		final JavaClassStruct rhs = (JavaClassStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(javaClass, rhs.javaClass)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(javaClass)
		                                                                .toString();
	}
}
