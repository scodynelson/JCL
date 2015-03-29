/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.declare;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class JavaClassNameDeclarationStruct implements DeclarationStruct {

	private static final long serialVersionUID = 1282436684482407122L;

	private final String className;

	public JavaClassNameDeclarationStruct(final String className) {
		this.className = className;
	}

	public String getClassName() {
		return className;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(className)
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
		final JavaClassNameDeclarationStruct rhs = (JavaClassNameDeclarationStruct) obj;
		return new EqualsBuilder().append(className, rhs.className)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(className)
		                                                                .toString();
	}
}
