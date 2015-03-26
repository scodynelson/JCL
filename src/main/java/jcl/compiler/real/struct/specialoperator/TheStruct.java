/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.types.typespecifiers.TypeSpecifier;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class TheStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -8054543185157500625L;

	private final TypeSpecifier valueType;

	private final LispStruct form;

	public TheStruct(final TypeSpecifier valueType, final LispStruct form) {
		this.valueType = valueType;
		this.form = form;
	}

	public TypeSpecifier getValueType() {
		return valueType;
	}

	public LispStruct getForm() {
		return form;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(valueType)
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
		final TheStruct rhs = (TheStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(valueType, rhs.valueType)
		                          .append(form, rhs.form)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(valueType)
		                                                                .append(form)
		                                                                .toString();
	}
}
