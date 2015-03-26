/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class GoStruct<T extends LispStruct> extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -4331758400526441262L;

	private final T tag;

	protected GoStruct(final T tag) {
		this.tag = tag;
	}

	public T getTag() {
		return tag;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(tag)
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
		final GoStruct<?> rhs = (GoStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(tag, rhs.tag)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(tag)
		                                                                .toString();
	}
}
