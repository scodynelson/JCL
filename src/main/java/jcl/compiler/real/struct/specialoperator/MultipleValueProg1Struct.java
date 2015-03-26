/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class MultipleValueProg1Struct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -1036080843176598388L;

	private final LispStruct firstForm;

	private final PrognStruct forms;

	public MultipleValueProg1Struct(final LispStruct firstForm, final PrognStruct forms) {
		this.firstForm = firstForm;
		this.forms = forms;
	}

	public LispStruct getFirstForm() {
		return firstForm;
	}

	public PrognStruct getForms() {
		return forms;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(firstForm)
		                            .append(forms)
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
		final MultipleValueProg1Struct rhs = (MultipleValueProg1Struct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(firstForm, rhs.firstForm)
		                          .append(forms, rhs.forms)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(firstForm)
		                                                                .append(forms)
		                                                                .toString();
	}
}
