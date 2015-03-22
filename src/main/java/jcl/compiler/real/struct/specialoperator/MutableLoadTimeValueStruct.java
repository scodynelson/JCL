/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class MutableLoadTimeValueStruct extends SpecialOperatorStruct implements LoadTimeValueStruct {

	private static final long serialVersionUID = 8088799347738800471L;

	private final LispStruct form;

	public MutableLoadTimeValueStruct(final LispStruct form) {
		this.form = form;
	}

	public LispStruct getForm() {
		return form;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(form)
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
		final MutableLoadTimeValueStruct rhs = (MutableLoadTimeValueStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(form, rhs.form)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(form)
		                                                                .toString();
	}
}
